//! Source location tracking.

use std::cmp::Ordering;
use std::ops::{Index, Range};
use std::path::{Path, PathBuf};

use crate::source::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileSpan {
    file: FileId,
    offset: u32,
    len: u16,
}

/// A line-column pair representing the start or end of a [`FileSpan`].
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FileId(u16);

#[derive(Clone, Debug)]
pub struct FileInfo {
    path: PathBuf,
    source_text: String,
    line_starts: Vec<u32>,
}

impl FileSpan {
    #[inline]
    pub fn new(file: FileId, offset: usize, len: usize) -> Self {
        let offset = u32::try_from(offset).expect("offset exceeds u32");
        let len = u16::try_from(len).expect("len exceeds u16");
        FileSpan { file, offset, len }
    }

    #[inline]
    pub fn from_span<T: Into<Span>>(file: FileId, span: T) -> Self {
        span.into().with_file(file)
    }

    #[inline]
    pub fn file(&self) -> FileId {
        self.file
    }

    #[inline]
    pub fn start(&self) -> usize {
        self.offset as usize
    }

    #[inline]
    pub fn end(&self) -> usize {
        self.start() + self.len()
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.len as usize
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    #[inline]
    pub fn contains(&self, offset: usize) -> bool {
        self.start() <= offset && offset < self.end()
    }

    #[inline]
    pub fn range(&self) -> Range<usize> {
        self.start()..self.end()
    }

    #[inline]
    pub fn file_info<'a>(&self, files: &'a FileSet) -> &'a FileInfo {
        &files[self.file]
    }

    #[inline]
    pub fn source_text<'a>(&self, files: &'a FileSet) -> &'a str {
        &files[self.file].source_text[self.range()]
    }

    /// Returns the line-column positions for the start (inclusive) and end
    /// (exclusive) of this span in the file.
    pub fn line_columns(&self, files: &FileSet) -> (LineColumn, LineColumn) {
        let file = self.file_info(files);
        debug_assert!(
            self.start() <= file.len() && self.end() <= file.len(),
            "out of bounds"
        );
        let start = file.line_column(self.start());
        let end = if self.is_empty() {
            start
        } else if !file
            .line_starts
            .get(start.line)
            .is_some_and(|&line_end| self.end() as u32 >= line_end)
        {
            // Start and end offsets are on the same line
            let chars = file.source_text[self.range()].chars().count();
            LineColumn::new(start.line, start.column + chars)
        } else {
            // Slower general case
            file.line_column(self.end())
        };
        (start, end)
    }
}

impl PartialOrd for FileSpan {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        (self.file == other.file).then(|| {
            self.offset
                .cmp(&other.offset)
                .then(self.len.cmp(&other.len))
        })
    }
}

impl LineColumn {
    #[inline]
    pub fn new(line: usize, column: usize) -> Self {
        LineColumn { line, column }
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

        let mut line_starts = vec![0];
        for (i, &b) in source_text.as_bytes().iter().enumerate() {
            if b == b'\n' {
                line_starts.push(i as u32 + 1);
            }
        }

        self.files.push(FileInfo {
            path,
            source_text,
            line_starts,
        });
        file
    }
}

impl FileId {
    #[inline]
    pub fn info<'a>(&self, files: &'a FileSet) -> &'a FileInfo {
        &files[*self]
    }
}

impl From<FileId> for u16 {
    #[inline]
    fn from(file: FileId) -> Self {
        file.0
    }
}

impl From<FileId> for usize {
    #[inline]
    fn from(file: FileId) -> Self {
        file.0 as usize
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

    #[inline]
    pub fn len(&self) -> usize {
        self.source_text.len()
    }

    #[inline]
    pub fn line_column(&self, offset: usize) -> LineColumn {
        assert!(offset <= self.len(), "offset out of bounds");
        let line = self.line_starts.partition_point(|&l| l <= offset as u32);
        let line_start = self.line_starts[line - 1] as usize;
        let column = (self.source_text[line_start..offset as usize].chars()).count() + 1;
        LineColumn { line, column }
    }
}

#[test]
fn line_column() {
    let mut files = FileSet::new();

    let f = files.insert("empty".into(), "".into());
    assert_eq!(
        (LineColumn::new(1, 1), LineColumn::new(1, 1)),
        FileSpan::from_span(f, 0..0).line_columns(&files)
    );

    let f = files.insert("utf-8".into(), "\nsüß\n".into());
    assert_eq!(
        (LineColumn::new(1, 1), LineColumn::new(2, 1)),
        FileSpan::from_span(f, 0..1).line_columns(&files)
    );
    assert_eq!(
        (LineColumn::new(2, 1), LineColumn::new(2, 4)),
        FileSpan::from_span(f, 1..6).line_columns(&files)
    );
    assert_eq!(
        (LineColumn::new(3, 1), LineColumn::new(3, 1)),
        FileSpan::from_span(f, 7..7).line_columns(&files)
    );
    assert_eq!(
        (LineColumn::new(1, 1), LineColumn::new(3, 1)),
        FileSpan::from_span(f, 0..7).line_columns(&files)
    );
}
