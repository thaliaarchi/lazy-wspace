use std::cell::{Ref, RefCell};
use std::io::{self, BufRead, BufReader, Read, Write};
use std::time::{Duration, Instant};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct IoRecorder {
    effects: RefCell<Vec<(Effect, Duration)>>,
    start: Instant,
}

#[derive(Debug)]
pub struct StdinRecorder<'a, R> {
    rec: &'a IoRecorder,
    inner: BufReader<R>,
    fill_buf_time: Option<Duration>,
}

#[derive(Debug)]
pub struct StdoutRecorder<'a> {
    rec: &'a IoRecorder,
}

#[derive(Debug)]
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
        StdinRecorder {
            rec: self,
            inner: BufReader::new(r),
            fill_buf_time: None,
        }
    }

    #[inline]
    pub fn stdout(&self) -> StdoutRecorder {
        StdoutRecorder { rec: self }
    }

    #[inline]
    pub fn stderr(&self) -> StderrRecorder {
        StderrRecorder { rec: self }
    }

    #[inline]
    pub fn effects(&self) -> Ref<'_, Vec<(Effect, Duration)>> {
        self.effects.borrow()
    }

    #[inline]
    pub fn into_effects(self) -> Vec<(Effect, Duration)> {
        self.effects.into_inner()
    }
}

impl Default for IoRecorder {
    #[inline]
    fn default() -> Self {
        IoRecorder::new()
    }
}

impl<R: Read> Read for StdinRecorder<'_, R> {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        assert!(
            self.fill_buf_time.is_none(),
            "consume must be called after fill_buf"
        );
        let mut effects = self.rec.effects.borrow_mut();
        let time = self.rec.start.elapsed();
        let res = self.inner.read(buf);
        let effect = Effect::Read(
            res.as_ref()
                .map(|&n| buf[..n].to_owned())
                .map_err(|err| err.kind()),
        );
        effects.push((effect, time));
        res
    }
}

impl<R: Read> BufRead for StdinRecorder<'_, R> {
    fn fill_buf(&mut self) -> io::Result<&[u8]> {
        if self.fill_buf_time.is_none() {
            let time = self.rec.start.elapsed();
            let res = self.inner.fill_buf();
            match &res {
                Ok(_) => self.fill_buf_time = Some(time),
                Err(err) => {
                    let mut effects = self.rec.effects.borrow_mut();
                    effects.push((Effect::Read(Err(err.kind())), time));
                }
            }
            res
        } else {
            Ok(self.inner.buffer())
        }
    }

    fn consume(&mut self, n: usize) {
        let time = self
            .fill_buf_time
            .expect("fill_buf must have been called before consume");
        let mut effects = self.rec.effects.borrow_mut();
        let effect = Effect::Read(Ok(self.inner.buffer()[..n].to_owned()));
        effects.push((effect, time));
        self.inner.consume(n);
        self.fill_buf_time = None;
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
