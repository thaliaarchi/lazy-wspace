use crate::hs;

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
pub enum Error {
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
    ///         - `toException` in class [`base:GHC.Exception.Type.Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L45-144)
    ///           ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:Exception))
    ///           - instance [`Exception base:GHC.IO.Exception.IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L342)
    ///           - [`base:GHC.Exception.Type.SomeException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L34-39)
    ///             ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:SomeException))
    ///       - [`ghc-prim:GHC.Prim.raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/compiler/GHC/Builtin/primops.txt.pp#L2611-2618)
    ///         ([docs](https://hackage.haskell.org/package/ghc-prim-0.11.0/docs/GHC-Prim.html#v:raiseIO-35-))
    ///         - [`stg_raiseIO#`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/rts/Exception.cmm#L643-646)
    /// - instance [`Show base:GHC.IO.Exception.IOException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L417-430)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Text-Show.html#t:Show))
    ///   - [`show base:GHC.IO.Exception.UserError`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/IO/Exception.hs#L389)
    UserError {
        /// The string passed to `fail`.
        description: String,
    },

    /// A call to `error`, which stops execution and displays an error message.
    ///
    /// # GHC definitions
    ///
    /// - [`base:GHC.Err.error`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Err.hs#L33-40)
    ///   ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#v:error))
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
    ///     - [`base:GHC.Exception.showCCSStack`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L88-90)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception.html#v:showCCSStack))
    ///     - [`base:GHC.Exception.ErrorCallWithLocation`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception.hs#L54-56)
    ///       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#v:ErrorCallWithLocation))
    ///     - `toException` in class [`base:GHC.Exception.Type.Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L45-144)
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
    Error {
        /// The string passed to `error`.
        description: String,
        /// The location stored in `ErrorCallWithLocation`.
        location: String,
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
    ///                   - `toException` in class [`base:GHC.Exception.Type.Exception`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L45-144)
    ///                     ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:Exception))
    ///                     - instance [`Exception base:GHC.Exception.Type.ArithException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L174)
    ///                     - `DivideByZero` in data [`base:GHC.Exception.Type.ArithException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L155-165)
    ///                       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Exception-Type.html#t:ArithException))
    ///                     - [`base:GHC.Exception.Type.SomeException`](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Exception/Type.hs#L34-39)
    ///                       ([docs](https://hackage.haskell.org/package/base-4.19.0.0/docs/Control-Exception.html#t:SomeException))
    DivZero,
}

impl hs::Show for Error {
    fn show(&self) -> String {
        match self {
            Error::UserError { description } => {
                if description.is_empty() {
                    "user error".to_string()
                } else {
                    format!("user error ({description})")
                }
            }
            Error::Error {
                description: _,
                location: _,
            } => todo!(),
            Error::DivZero => "divide by zero".to_owned(),
        }
    }
}

/// A partial call stack obtained by `HasCallStack`.
///
/// Represents [`GHC.Stack.CallStack`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:CallStack)
/// ([source](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L74-144)).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CallStack {
    /// The `String` is the name of the function that was called and the
    /// `SrcLoc` is the call-site. The list is ordered with the most recently
    /// called function last.
    pub entries: Vec<(String, SrcLoc)>,
    /// A frozen call stack cannot be pushed to.
    pub frozen: bool,
}

/// A single location in Haskell source code.
///
/// Represents [`GHC.Stack.SrcLoc`](https://hackage.haskell.org/package/base-4.19.0.0/docs/GHC-Stack.html#t:SrcLoc)
/// ([source](https://gitlab.haskell.org/ghc/ghc/-/blob/ghc-9.8.1-release/libraries/base/GHC/Stack/Types.hs#L210-221)).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SrcLoc {
    pub package: String,
    pub module: String,
    pub file: String,
    pub start_line: i64,
    pub start_col: i64,
    pub end_line: i64,
    pub end_col: i64,
}
