//! Source location tracking.

use std::cmp::Ordering;
use std::num::NonZeroU16;
use std::ops::Index;
use std::path::{Path, PathBuf};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    file: FileId,
    offset: BytePos,
    len: NonZeroU16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(u16);

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct BytePos(u32);

/// A line-column pair representing the start or end of a [`Span`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LineColumn {
    /// The 1-indexed line in the source file on which the span starts or ends
    /// (inclusive).
    pub line: usize,
    /// The 1-indexed column (in UTF-8 characters) in the source file on which
    /// the span starts or ends (inclusive).
    pub column: usize,
}

#[derive(Clone, Debug)]
pub struct FileSet {
    files: Vec<FileInfo>,
}

#[derive(Clone, Debug)]
pub struct FileInfo {
    path: PathBuf,
    source_text: String,
    lines: Vec<BytePos>,
}

impl Span {
    #[inline]
    pub fn new(file: FileId, offset: usize, len: usize) -> Self {
        let offset = BytePos(u32::try_from(offset).expect("offset exceeds u32"));
        let len = u16::try_from(len)
            .expect("len exceeds u16")
            .try_into()
            .expect("len is 0");
        Span { file, offset, len }
    }

    #[inline]
    pub fn file(&self) -> FileId {
        self.file
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.offset.0 as usize
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.start() + self.len()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len.get() as usize
    }

    #[inline]
    pub fn file_info<'a>(&self, files: &'a FileSet) -> &'a FileInfo {
        &files[self.file]
    }

    #[inline]
    pub fn source_text<'a>(&self, files: &'a FileSet) -> &'a str {
        &files[self.file].source_text[self.start()..self.end()]
    }

    pub fn bounds(&self, files: &FileSet) -> (LineColumn, LineColumn) {
        let file = self.file_info(files);
        let start = {
            let line = file.lines.partition_point(|&line| line <= self.offset);
            let line_start = file.lines[line - 1].0 as usize;
            let column = (file.source_text[line_start..self.offset.0 as usize].chars()).count() + 1;
            LineColumn { line, column }
        };
        let end = {
            let offset = BytePos(self.offset.0 + self.len.get() as u32);
            let line = file.lines.partition_point(|&line| line < offset);
            let line_start = file.lines[line - 1].0 as usize;
            let column = (file.source_text[line_start..offset.0 as usize].chars()).count() + 1;
            LineColumn { line, column }
        };
        (start, end)
    }
}

impl PartialOrd for LineColumn {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for LineColumn {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.line
            .cmp(&other.line)
            .then(self.column.cmp(&other.column))
    }
}

impl FileSet {
    #[inline]
    pub fn new() -> Self {
        FileSet { files: Vec::new() }
    }

    pub fn insert(&mut self, path: PathBuf, source_text: String) -> FileId {
        assert!(
            u16::try_from(self.files.len()).is_ok_and(|l| l < u16::MAX),
            "number of files exceeds u16",
        );
        assert!(
            u32::try_from(source_text.len()).is_ok_and(|l| l < u32::MAX),
            "source text length exceeds u32",
        );
        let file = FileId(self.files.len() as u16);

        let mut lines = vec![BytePos(0)];
        let mut len = 0;
        for &b in source_text.as_bytes() {
            len += 1;
            if b == b'\n' {
                lines.push(BytePos(len));
            }
        }

        self.files.push(FileInfo {
            path,
            source_text,
            lines,
        });
        file
    }
}

impl Index<FileId> for FileSet {
    type Output = FileInfo;

    #[inline]
    fn index(&self, index: FileId) -> &FileInfo {
        &self.files[index.0 as usize]
    }
}

impl FileInfo {
    #[inline]
    pub fn path(&self) -> &Path {
        &self.path
    }

    #[inline]
    pub fn source_text(&self) -> &str {
        &self.source_text
    }
}
