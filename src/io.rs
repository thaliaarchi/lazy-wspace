use std::cell::RefCell;
use std::io::{self, BufRead, ErrorKind, Read, Write};
use std::time::{Duration, Instant};

use arrayvec::ArrayVec;

use crate::error::Error;

#[repr(transparent)]
#[derive(Debug, PartialEq, Eq)]
pub struct CharReader<R> {
    inner: R,
}

impl<R: BufRead> CharReader<R> {
    #[inline]
    pub fn new(r: R) -> Self {
        CharReader { inner: r }
    }

    pub fn read_char(&mut self) -> Result<char, Error> {
        let mut partial = ArrayVec::<u8, 4>::new();
        loop {
            let buf = self.inner.fill_buf()?;
            if buf.len() == 0 {
                return Err(Error::ReadEof);
            }

            if partial.len() != 0 {
                let extend = (partial.capacity() - partial.len()).min(buf.len());
                partial.try_extend_from_slice(&buf[..extend]).unwrap();

                let (ch, n) = bstr::decode_utf8(&partial);
                self.inner.consume(n);
                if ch.is_none() && n == partial.len() {
                    continue;
                }
                return ch.ok_or(Error::ReadInvalidUtf8);
            }

            let (ch, n) = bstr::decode_utf8(buf);
            if ch.is_none() && n == buf.len() {
                partial.try_extend_from_slice(buf).unwrap();
                self.inner.consume(n);
                continue;
            }
            self.inner.consume(n);
            return ch.ok_or(Error::ReadInvalidUtf8);
        }
    }

    pub fn read_line(&mut self) -> Result<String, Error> {
        let mut line = String::new();
        match self.inner.read_line(&mut line) {
            Ok(_) => Ok(line),
            Err(err) if err.kind() == ErrorKind::InvalidData => Err(Error::ReadInvalidUtf8),
            Err(err) => Err(err.into()),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IoRecorder {
    effects: RefCell<Vec<(Effect, Duration)>>,
    start: Instant,
}

#[derive(Clone, Debug)]
pub struct StdinRecorder<'a, R> {
    r: R,
    rec: &'a IoRecorder,
}

#[derive(Clone, Debug)]
pub struct StdoutRecorder<'a> {
    rec: &'a IoRecorder,
}

#[derive(Clone, Debug)]
pub struct StderrRecorder<'a> {
    rec: &'a IoRecorder,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Effect {
    Read(Result<Vec<u8>, io::ErrorKind>),
    WriteOut(Vec<u8>),
    WriteErr(Vec<u8>),
    FlushOut,
    FlushErr,
}

impl IoRecorder {
    #[inline]
    pub fn new() -> Self {
        IoRecorder {
            effects: RefCell::new(Vec::new()),
            start: Instant::now(),
        }
    }

    #[inline]
    pub fn stdin<R: Read>(&self, r: R) -> StdinRecorder<R> {
        StdinRecorder { r, rec: self }
    }

    #[inline]
    pub fn stdout(&self) -> StdoutRecorder {
        StdoutRecorder { rec: self }
    }

    #[inline]
    pub fn stderr(&self) -> StderrRecorder {
        StderrRecorder { rec: self }
    }
}

impl<R: Read> Read for StdinRecorder<'_, R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let mut effects = self.rec.effects.borrow_mut();
        let time = self.rec.start.elapsed();
        let res = self.r.read(buf);
        let effect = Effect::Read(
            res.as_ref()
                .map(|&n| buf[..n].to_owned())
                .map_err(|err| err.kind()),
        );
        effects.push((effect, time));
        res
    }
}

impl Write for StdoutRecorder<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut effects = self.rec.effects.borrow_mut();
        let time = self.rec.start.elapsed();
        effects.push((Effect::WriteOut(buf.to_owned()), time));
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        let mut effects = self.rec.effects.borrow_mut();
        let time = self.rec.start.elapsed();
        effects.push((Effect::FlushOut, time));
        Ok(())
    }
}

impl Write for StderrRecorder<'_> {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        let mut effects = self.rec.effects.borrow_mut();
        let time = self.rec.start.elapsed();
        effects.push((Effect::WriteErr(buf.to_owned()), time));
        Ok(buf.len())
    }

    fn flush(&mut self) -> io::Result<()> {
        let mut effects = self.rec.effects.borrow_mut();
        let time = self.rec.start.elapsed();
        effects.push((Effect::FlushErr, time));
        Ok(())
    }
}
