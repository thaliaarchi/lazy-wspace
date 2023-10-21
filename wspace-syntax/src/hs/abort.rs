use std::fmt::{self, Display, Formatter};

use crate::hs::Show;

/// A representation of Haskell errors and exceptions.
///
/// It does not attempt to represent all Haskell errors and exceptions—only
/// those that may be encountered in the execution of a Whitespace program by
/// the reference interpreter.
///
/// In Haskell, [errors and exceptions](https://wiki.haskell.org/Error_vs._Exception)
/// are distinct. Exceptions can be manipulated as values and signify
/// recoverable situations, while errors are for programming errors and abort
/// the program. In the reference interpreter, however, no exceptions or errors
/// are caught, so they act functionally the same, aborting the program, and are
/// combined in this type.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Abort {
    /// A user-created exception from the `IO` monad.
    ///
    /// Specifically, it is an `IOError` constructed with the `fail` method of
    /// the `MonadFail` instance for `IO`.
    ///
    /// # GHC definitions
    ///
    /// - `fail` in class [`base:Control.Monad.MonadFail`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/Control/Monad/Fail.hs#L43-70)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Monad.html#t:MonadFail))
    ///   - instance [`base:Control.Monad.MonadFail IO`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/Control/Monad/Fail.hs#L83-84)
    ///     - [`base:GHC.Base.failIO`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Base.hs#L1764-1765)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Base.html#v:failIO))
    ///       - [`base:GHC.IO.mkUserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO.hs#L479-480)
    ///         ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-IO.html#v:mkUserError))
    ///         - [`base:GHC.IO.Exception.userError`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L402-411)
    ///           ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-IO-Exception.html#v:userError))
    ///           - [`base:GHC.IO.Exception.IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L326-339)
    ///             ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-IO-Exception.html#t:IOException))
    ///         - instance [`Exception base:GHC.IO.Exception.IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L342)
    ///       - [`ghc-prim:GHC.Prim.raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/compiler/GHC/Builtin/primops.txt.pp#L2611-2618)
    ///         ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim.html#v:raiseIO-35-))
    ///         - [`stg_raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/rts/Exception.cmm#L643-646)
    /// - instance [`Show base:GHC.IO.Exception.IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L417-430)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Text-Show.html#t:Show))
    ///   - instance [`Show base:GHC.IO.Exception.IOErrorType`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L389)
    UserError {
        /// The string passed to `fail`.
        description: String,
    },

    /// A call to `error` or `errorWithoutStackTrace`, which stops execution and
    /// displays an error message.
    ///
    /// # GHC definitions
    ///
    /// - [`base:GHC.Err.error`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Err.hs#L33-40)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Err.html#v:error))
    ///   - [`base:GHC.Stack.HasCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L64-72)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:HasCallStack),
    ///     [guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/callstack.html))
    ///   - [`base:GHC.Stack.CallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L74-144)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:CallStack))
    ///     - [`base:GHC.Stack.SrcLoc`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L210-221)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:SrcLoc))
    ///   - [`base:GHC.Exception.errorCallWithCallStackException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L79-86)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:errorCallWithCallStackException))
    ///     - [`base:GHC.Stack.currentCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/CCS.hsc#L115-126)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#v:currentCallStack))
    ///       - [`base:GHC.Stack.getCurrentCCS`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/CCS.hsc#L57-64)
    ///         ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#v:getCurrentCCS))
    ///       - [`base:GHC.Stack.ccsToStrings`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/CCS.hsc#L128-142)
    ///         ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#v:ccsToStrings))
    ///     - [`base:GHC.Exception.prettyCallStackLines`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L113-119)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:prettyCallStackLines))
    ///       - [`base:GHC.Stack.getCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L146-155)
    ///         ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#v:getCallStack))
    ///       - [`base:GHC.Exception.prettySrcLoc`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L95-105)
    ///         ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:prettySrcLoc))
    ///     - [`base:GHC.Exception.showCCSStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L88-90)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:showCCSStack))
    ///     - [`base:GHC.Exception.ErrorCallWithLocation`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L54-63)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#v:ErrorCallWithLocation))
    ///     - instance [`Exception base:GHC.Exception.ErrorCall`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L68)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:Exception))
    ///     - [`base:GHC.IO.unsafeDupablePerformIO`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Unsafe.hs#L129-145)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-IO.html#v:unsafeDupablePerformIO))
    ///       - [`ghc-prim:GHC.Magic.runRW#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-prim/GHC/Magic.hs#L107-119)
    ///         ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Magic.html#v:runRW-35-))
    ///       - [`ghc-prim:GHC.Magic.lazy`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-prim/GHC/Magic.hs#L64-86)
    ///         ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Magic.html#v:lazy))
    ///   - [`ghc-prim:GHC.Prim.raise#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/compiler/GHC/Builtin/primops.txt.pp#L2570-2585)
    ///     ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim.html#v:raise-35-))
    ///     - [`stg_raise#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/rts/Exception.cmm#L451-641)
    /// - [`base:GHC.Err.errorWithoutStackTrace`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Err.hs#L42-47)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Err.html#v:errorWithoutStackTrace))
    ///   - [`base:GHC.Exception.errorCallException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L76-77)
    ///     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:errorCallException))
    ///     - [`base:GHC.Exception.ErrorCall`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L54-63)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:ErrorCall))
    ///   - `ghc-prim:GHC.Prim.raise#` (see above)
    /// - instance [`Show base:GHC.Exception.ErrorCall`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L71-74)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Text-Show.html#t:Show))
    ErrorCall {
        /// The string passed to `error` or `errorWithoutStackTrace`.
        description: String,
        /// The call stack from `base:GHC.Stack.getCallStack` at the call to
        /// `error`. When raising from `errorWithoutStackTrace`, the call stack
        /// is empty.
        call_stack: CallStack,
    },

    /// A divide by zero exception raised by operations on `Integer` or
    /// `BigNat`.
    ///
    /// # GHC definitions
    ///
    /// - [`base:GHC.Integer.divInteger` and `modInteger`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Integer.hs#L189-212)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Integer.html#v:divInteger))
    ///   - [`ghc-bignum:GHC.Num.Integer.integerDiv` and `integerMod`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-bignum/src/GHC/Num/Integer.hs#L896-909)
    ///     ([docs](https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-Integer.html#v:integerDiv))
    ///     - [`ghc-bignum:GHC.Num.Integer.integerDivMod#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-bignum/src/GHC/Num/Integer.hs#L872-885)
    ///       ([docs](https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-Integer.html#v:integerDivMod-35-))
    ///       - [`ghc-bignum:GHC.Num.Integer.integerQuotRem#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-bignum/src/GHC/Num/Integer.hs#L786-822)
    ///         ([docs](https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-Integer.html#v:integerQuotRem-35-))
    ///         - [`ghc-bignum:GHC.Num.Primitives.raiseDivZero`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-bignum/src/GHC/Num/Primitives.hs#L599-637)
    ///           ([docs](https://hackage.haskell.org/package/ghc-bignum-1.3/docs/GHC-Num-Primitives.html#v:raiseDivZero))
    ///           - [`ghc-prim:GHC.Prim.Exception.raiseDivZero`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/ghc-prim/GHC/Prim/Exception.hs#L20-41)
    ///             ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim-Exception.html#v:raiseDivZero))
    ///             - [`ghc-prim:GHC.Prim.raiseDivZero#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/compiler/GHC/Builtin/primops.txt.pp#L2603-2609)
    ///               ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim.html#v:raiseDivZero-35-))
    ///               - [`stg_raiseDivZero#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/rts/Exception.cmm#L649-652)
    ///                 - [`base:GHC.Exception.Type.divZeroException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L168)
    ///                   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception-Type.html#v:divZeroException))
    ///                   - instance [`Exception base:GHC.Exception.Type.ArithException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L174)
    ///                     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:Exception))
    ///                   - [`base:GHC.Exception.Type.DivideByZero`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L155-165)
    ///                     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception-Type.html#v:DivideByZero))
    /// - instance [`Show base:GHC.Exception.Type.ArithException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L181)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Text-Show.html#t:Show))
    DivZeroException,
}

impl Abort {
    pub fn user_error<T: Into<String>>(description: T) -> Self {
        Abort::UserError {
            description: description.into(),
        }
    }

    pub fn error<T: Into<String>>(description: T, call_stack: CallStack) -> Self {
        Abort::ErrorCall {
            description: description.into(),
            call_stack,
        }
    }

    pub fn error_without_stack_trace<T: Into<String>>(description: T) -> Self {
        Abort::ErrorCall {
            description: description.into(),
            call_stack: CallStack::new(),
        }
    }
}

impl Show for Abort {
    fn show(&self) -> String {
        match self {
            // Models instance [`Show base:GHC.IO.Exception.IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L417-430),
            // specialized for exceptions created by `userError`.
            Abort::UserError { description } => {
                if description.is_empty() {
                    "user error".to_owned()
                } else {
                    format!("user error ({description})")
                }
            }
            // Models instance [`Show base:GHC.Exception.ErrorCall`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L71-74).
            Abort::ErrorCall {
                description,
                call_stack,
            } => {
                if call_stack.is_empty() {
                    description.clone()
                } else {
                    format!("{description}\n{call_stack}")
                }
            }
            // Models instance [`Show base:GHC.Exception.Type.ArithException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L181),
            // specialized for exceptions created by `divZeroException`.
            Abort::DivZeroException => "divide by zero".to_owned(),
        }
    }
}

/// A partial call stack obtained by `HasCallStack`.
///
/// The string is the name of the function that was called and the `SrcLoc` is
/// the call-site. The list is ordered with the most recently called function
/// last.
///
/// Models [`base:GHC.Stack.CallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L74-144)
/// ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:CallStack)),
/// but omits freezing.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallStack(pub Vec<(&'static str, SrcLoc)>);

/// A single location in Haskell source code.
///
/// Models [`base:GHC.Stack.SrcLoc`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L210-221)
/// ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:SrcLoc)),
/// but it omits `srcLocEndLine` and `srcLocEndCol`, because they are unused in
/// all of GHC, and represents `srcLocStartLine` and `srcLocStartCol` as `u64`
/// to hold the range of `Int`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SrcLoc {
    pub package: &'static str,
    pub module: &'static str,
    pub file: &'static str,
    pub line: u64,
    pub col: u64,
}

impl CallStack {
    /// Models [base:GHC.Stack.emptyCallStack](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L192-197)
    /// ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#v:emptyCallStack)).
    pub fn new() -> Self {
        CallStack(Vec::new())
    }

    /// Models [base:GHC.Stack.pushCallStack](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L180-189)
    /// ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#v:pushCallStack)).
    pub fn push(&mut self, func: &'static str, loc: SrcLoc) {
        self.0.push((func, loc));
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

/// Models [`base:GHC.Exception.prettyCallStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L107-119)
/// ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:prettyCallStack)).
impl Display for CallStack {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.is_empty() {
            f.write_str("CallStack (from HasCallStack):")?;
            for (func, loc) in self.0.iter().rev() {
                write!(f, "\n  {func}, called at {loc}")?;
            }
        }
        Ok(())
    }
}

/// Models [`base:GHC.Exception.prettySrcLoc`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L95-105)
/// ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:prettySrcLoc)).
impl Display for SrcLoc {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}:{}:{} in {}:{}",
            self.file, self.line, self.col, self.package, self.module,
        )
    }
}

#[cfg(test)]
macro_rules! SrcLoc(($file:literal : $line:literal : $col:literal in $package:literal : $module:literal) => {
    SrcLoc {
        package: $package,
        module: $module,
        file: $file,
        line: $line,
        col: $col,
    }
});

/// ```haskell
/// prettyCallStack
///     (pushCallStack ("fn2", (SrcLoc "p2" "m2" "f2" 5 6 7 8))
///     (pushCallStack ("fn1", (SrcLoc "p1" "m1" "f1" 1 2 3 4))
///      emptyCallStack))
/// ```
#[test]
fn pretty_call_stack() {
    let pretty = "CallStack (from HasCallStack):\n  fn2, called at f2:5:6 in p2:m2\n  fn1, called at f1:1:2 in p1:m1";
    let mut stk = CallStack::new();
    stk.push("fn1", SrcLoc!("f1":1:2 in "p1":"m1"));
    stk.push("fn2", SrcLoc!("f2":5:6 in "p2":"m2"));
    assert_eq!(pretty, format!("{stk}"));
}
