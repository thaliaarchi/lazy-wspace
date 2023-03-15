use std::io::{BufRead, ErrorKind};

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
