//  ERRORS.rs
//    by Lut99
//
//  Created:
//    26 Aug 2022, 18:01:09
//  Last edited:
//    31 Jan 2024, 11:36:09
//  Auto updated?
//    Yes
//
//  Description:
//!   Defines errors that occur in the `brane-exe` crate.
//

use std::error::Error;
use std::fmt::{Display, Formatter, Result as FResult};
use std::path::PathBuf;

use brane_ast::func_id::FunctionId;
use brane_ast::{DataType, MergeStrategy};
use console::style;
use enum_debug::EnumDebug as _;
use specifications::data::DataName;
use specifications::version::Version;

use crate::pc::ProgramCounter;

/***** HELPER FUNCTIONS *****/
/// Prints the given error (of an instruction) to stderr.
///
/// # Arguments
/// - `edge`: The edge index to print.
/// - `instr`: The instruction index to print.
/// - `err`: The Error to print.
///
/// # Returns
/// Nothing, but does print the err to stderr.
fn prettyprint_err_instr(pc: ProgramCounter, instr: Option<usize>, err: &dyn Error) {
    // Print the thing
    eprintln!(
        "{}: {}: {}",
        style(format!("{}{}", pc, if let Some(instr) = instr { format!(":{instr}") } else { String::new() })).bold(),
        style("error").red().bold(),
        err
    );

    // Done
}

/// Prints the given error to stderr.
///
/// # Arguments
/// - `edge`: The edge index to print.
/// - `err`: The Error to print.
///
/// # Returns
/// Nothing, but does print the err to stderr.
fn prettyprint_err(pc: ProgramCounter, err: &dyn Error) {
    // Print the thing
    eprintln!("{}: {}: {}", style(format!("{pc}")).bold(), style("error").red().bold(), err);

    // Done
}

/***** AUXILLARY *****/
/// Trait that makes printing shit a bit easier.
pub trait ReturnEdge {
    /// The return type
    type Ret;

    /// Maps this result to a VmError that has only an edge.
    ///
    /// # Arguments
    /// - `edge`: The edge to insert.
    fn to(self, pc: ProgramCounter) -> Result<Self::Ret, VmError>;

    /// Maps this result to a VmError that has some instructions.
    ///
    /// # Arguments
    /// - `edge`: The edge to insert.
    /// - `instr`: The instruction to insert.
    fn to_instr(self, pc: ProgramCounter, instr: usize) -> Result<Self::Ret, VmError>;
}

impl<T> ReturnEdge for Result<T, StackError> {
    /// The return type
    type Ret = T;

    /// Maps this result to a VmError that has only an edge.
    ///
    /// # Arguments
    /// - `edge`: The edge to insert.
    fn to(self, pc: ProgramCounter) -> Result<Self::Ret, VmError> {
        match self {
            Ok(val) => Ok(val),
            Err(err) => Err(VmError::StackError { pc, instr: None, err }),
        }
    }

    /// Maps this result to a VmError that has some instructions.
    ///
    /// # Arguments
    /// - `edge`: The edge to insert.
    /// - `instr`: The instruction to insert.
    fn to_instr(self, pc: ProgramCounter, instr: usize) -> Result<Self::Ret, VmError> {
        match self {
            Ok(val) => Ok(val),
            Err(err) => Err(VmError::StackError { pc, instr: Some(instr), err }),
        }
    }
}

/***** LIBRARY *****/
/// Defines errors that relate to the values.
#[derive(Debug)]
pub enum ValueError {
    /// Failed to parse the Value from the given `serde_json::Value` object.
    JsonError { err: serde_json::Error },

    /// Failed to cast a value from one type to another.
    CastError { got: DataType, target: DataType },
}

impl Display for ValueError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use ValueError::*;
        match self {
            JsonError { err } => write!(f, "Cannot parse the given JSON value to a Value: {err}"),

            CastError { got, target } => write!(f, "Cannot cast a value of type {got} to {target}"),
        }
    }
}

impl Error for ValueError {}

/// Defines errors that relate to the stack.
#[derive(Debug)]
pub enum StackError {
    /// The stack overflowed :(
    StackOverflowError { size: usize },
}

impl Display for StackError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use StackError::*;
        match self {
            StackOverflowError { size } => write!(f, "Stack overflow occurred (has space for {size} values)"),
        }
    }
}

impl Error for StackError {}

/// Defines errors that relate to the frame stack.
#[derive(Debug)]
pub enum FrameStackError {
    /// The FrameStack was empty but still popped.
    EmptyError,
    /// The FrameStack overflowed.
    OverflowError { size: usize },

    /// A certain variable was not declared before it was set/gotten.
    UndeclaredVariable { name: String },
    /// A certain variable was declared twice.
    DuplicateDeclaration { name: String },
    /// A certain variable was undeclared without it ever being declared.
    UndeclaredUndeclaration { name: String },
    /// The given variable was declared but not initialized.
    UninitializedVariable { name: String },
    /// The new value of a variable did not match the expected.
    VarTypeError { name: String, got: DataType, expected: DataType },
    /// The given variable was not known in the FrameStack.
    VariableNotInScope { name: String },
}

impl Display for FrameStackError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use FrameStackError::*;
        match self {
            EmptyError => write!(f, "Frame stack empty"),
            OverflowError { size } => write!(f, "Frame stack overflow occurred (has space for {size} frames/nested calls)"),

            UndeclaredVariable { name } => write!(f, "Undeclared variable '{name}'"),
            DuplicateDeclaration { name } => write!(f, "Cannot declare variable '{name}' if it is already declared"),
            UndeclaredUndeclaration { name } => write!(f, "Cannot undeclare variable '{name}' that was never declared"),
            UninitializedVariable { name } => write!(f, "Uninitialized variable '{name}'"),
            VarTypeError { name, got, expected } => write!(f, "Cannot assign value of type {got} to variable '{name}' of type {expected}"),
            VariableNotInScope { name } => write!(f, "Variable '{name}' is declared but not currently in scope"),
        }
    }
}

impl Error for FrameStackError {}

/// Defines errors that relate to the variable register.
#[derive(Debug)]
pub enum VarRegError {
    /// The given variable was already declared.
    DuplicateDeclaration { id: usize, old_name: String, old_type: DataType, new_name: String, new_type: DataType },
    /// The given variable was not declared.
    UndeclaredVariable { id: usize },
    /// The given variable was declared but never initialized.
    UninitializedVariable { id: usize },
}

impl Display for VarRegError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use VarRegError::*;
        match self {
            DuplicateDeclaration { id, old_name, old_type, new_name, new_type } => {
                write!(f, "Variable {id} was already declared before (old '{old_name}: {old_type}', new '{new_name}: {new_type}')")
            },
            UndeclaredVariable { id } => write!(f, "Variable {id} was not declared"),
            UninitializedVariable { id } => write!(f, "Variable {id} was not initialized"),
        }
    }
}

impl Error for VarRegError {}

/// Defines errors that relate to a VM's execution.
#[derive(Debug)]
pub enum VmError {
    // /// Failed to read the given reader.
    // ReaderReadError{ err: std::io::Error },
    // /// Failed to compile the source (but already printed why).
    // CompileError{ errs: Vec<brane_ast::Error> },
    /// An error occurred while instantiating the custom state.
    GlobalStateError { err: Box<dyn Send + Sync + Error> },

    /// The given function pointer was out-of-bounds for the given workflow.
    UnknownFunction { func: FunctionId },
    /// The given program counter was out-of-bounds for the given function.
    PcOutOfBounds { func: FunctionId, edges: usize, got: usize },

    /// We expected there to be a value on the stack but there wasn't.
    EmptyStackError { pc: ProgramCounter, instr: Option<usize>, expected: DataType },
    /// The value on top of the stack was of unexpected data type.
    StackTypeError { pc: ProgramCounter, instr: Option<usize>, got: DataType, expected: DataType },
    /// The two values on top of the stack (in a lefthand-side, righthand-side fashion) are of incorrect data types.
    StackLhsRhsTypeError { pc: ProgramCounter, instr: usize, got: (DataType, DataType), expected: DataType },
    /// A value in an Array was incorrectly typed.
    ArrayTypeError { pc: ProgramCounter, instr: usize, got: DataType, expected: DataType },
    /// A value in an Instance was incorrectly typed.
    InstanceTypeError { pc: ProgramCounter, instr: usize, class: String, field: String, got: DataType, expected: DataType },
    /// Failed to perform a cast instruction.
    CastError { pc: ProgramCounter, instr: usize, err: ValueError },
    /// The given integer was out-of-bounds for an array with given length.
    ArrIdxOutOfBoundsError { pc: ProgramCounter, instr: usize, got: i64, max: usize },
    /// The given field was not present in the given class
    ProjUnknownFieldError { pc: ProgramCounter, instr: usize, class: String, field: String },
    /// Could not declare the variable.
    VarDecError { pc: ProgramCounter, instr: usize, err: FrameStackError },
    /// Could not un-declare the variable.
    VarUndecError { pc: ProgramCounter, instr: usize, err: FrameStackError },
    /// Could not get the value of a variable.
    VarGetError { pc: ProgramCounter, instr: usize, err: FrameStackError },
    /// Could not set the value of a variable.
    VarSetError { pc: ProgramCounter, instr: usize, err: FrameStackError },

    /// Failed to spawn a new thread.
    SpawnError { pc: ProgramCounter, err: tokio::task::JoinError },
    /// One of the branches of a parallel returned an invalid type.
    BranchTypeError { pc: ProgramCounter, branch: usize, got: DataType, expected: DataType },
    /// The branch' type does not match that of the current merge strategy at all
    IllegalBranchType { pc: ProgramCounter, branch: usize, merge: MergeStrategy, got: DataType, expected: DataType },
    /// One of a function's arguments was of an incorrect type.
    FunctionTypeError { pc: ProgramCounter, name: String, arg: usize, got: DataType, expected: DataType },
    /// We got told to run a function but do not know where.
    UnresolvedLocation { pc: ProgramCounter, name: String },
    /// The given input (dataset, result) was not there as possible option for the given task.
    UnknownInput { pc: ProgramCounter, task: String, name: DataName },
    /// The given input (dataset, result) was not yet planned at the time of execution.
    UnplannedInput { pc: ProgramCounter, task: String, name: DataName },
    // /// The given dataset was not locally available by the time it has to be executed.
    // UnavailableDataset{ pc: ProgramCounter, name: DataName },
    /// Attempted to call a function but the framestack thought otherwise.
    FrameStackPushError { pc: ProgramCounter, err: FrameStackError },
    /// Attempted to call a function but the framestack was empty.
    FrameStackPopError { pc: ProgramCounter, err: FrameStackError },
    /// The return type of a function was not correct
    ReturnTypeError { pc: ProgramCounter, got: DataType, expected: DataType },

    /// There was a type mismatch in a task call.
    TaskTypeError { pc: ProgramCounter, name: String, arg: usize, got: DataType, expected: DataType },

    /// A given asset was not found at all.
    UnknownData { pc: ProgramCounter, name: String },
    /// A given intermediate result was not found at all.
    UnknownResult { pc: ProgramCounter, name: String },
    /// The given package was not known.
    UnknownPackage { pc: ProgramCounter, name: String, version: Version },
    /// Failed to serialize the given argument list.
    ArgumentsSerializeError { pc: ProgramCounter, err: serde_json::Error },

    /// An error that relates to the stack.
    StackError { pc: ProgramCounter, instr: Option<usize>, err: StackError },
    /// A Vm-defined error.
    Custom { pc: ProgramCounter, err: Box<dyn Send + Sync + Error> },
}

impl VmError {
    /// Prints the VM error neatly to stderr.
    #[inline]
    pub fn prettyprint(&self) {
        use VmError::*;
        match self {
            // ReaderReadError{ .. }  => eprintln!("{}", self),
            // CompileError{ .. }     => eprintln!("{}", self),
            GlobalStateError { .. } => eprintln!("{self}"),

            UnknownFunction { .. } => eprintln!("{self}"),
            PcOutOfBounds { .. } => eprintln!("{self}"),

            EmptyStackError { pc, instr, .. } => prettyprint_err_instr(*pc, *instr, self),
            StackTypeError { pc, instr, .. } => prettyprint_err_instr(*pc, *instr, self),
            StackLhsRhsTypeError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            ArrayTypeError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            InstanceTypeError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            CastError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            ArrIdxOutOfBoundsError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            ProjUnknownFieldError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            VarDecError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            VarUndecError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            VarGetError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),
            VarSetError { pc, instr, .. } => prettyprint_err_instr(*pc, Some(*instr), self),

            SpawnError { pc, .. } => prettyprint_err(*pc, self),
            BranchTypeError { pc, .. } => prettyprint_err(*pc, self),
            IllegalBranchType { pc, .. } => prettyprint_err(*pc, self),
            FunctionTypeError { pc, .. } => prettyprint_err(*pc, self),
            UnresolvedLocation { pc, .. } => prettyprint_err(*pc, self),
            UnknownInput { pc, .. } => prettyprint_err(*pc, self),
            UnplannedInput { pc, .. } => prettyprint_err(*pc, self),
            // UnavailableDataset{ pc, .. }  => prettyprint_err(*pc, self),
            FrameStackPushError { pc, .. } => prettyprint_err(*pc, self),
            FrameStackPopError { pc, .. } => prettyprint_err(*pc, self),
            ReturnTypeError { pc, .. } => prettyprint_err(*pc, self),

            TaskTypeError { pc, .. } => prettyprint_err(*pc, self),

            UnknownData { pc, .. } => prettyprint_err(*pc, self),
            UnknownResult { pc, .. } => prettyprint_err(*pc, self),
            UnknownPackage { pc, .. } => prettyprint_err(*pc, self),
            ArgumentsSerializeError { pc, .. } => prettyprint_err(*pc, self),

            StackError { pc, instr, .. } => prettyprint_err_instr(*pc, *instr, self),
            Custom { pc, .. } => prettyprint_err(*pc, self),
        }
    }
}

impl Display for VmError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use VmError::*;
        match self {
            // ReaderReadError { err } => write!(f, "Failed to read from the given reader: {}", err),
            // CompileError{ .. }      => write!(f, "Could not compile the given source text (see output above)"),
            GlobalStateError { err } => write!(f, "Could not create custom state: {err}"),

            UnknownFunction { func } => write!(f, "Unknown function {func}"),
            PcOutOfBounds { func, edges, got } => write!(f, "Edge index {got} is out-of-bounds for function {func} with {edges} edges"),

            EmptyStackError { expected, .. } => write!(f, "Expected a value of type {expected} on the stack, but stack was empty"),
            StackTypeError { got, expected, .. } => write!(f, "Expected a value of type {expected} on the stack, but got a value of type {got}"),
            StackLhsRhsTypeError { got, expected, .. } => write!(
                f,
                "Expected a lefthand-side and righthand-side of (the same) {} type on the stack, but got types {} and {}, respectively (remember \
                 that rhs is on top)",
                expected, got.0, got.1
            ),
            ArrayTypeError { got, expected, .. } => {
                write!(f, "Expected an array element of type {expected} on the stack, but got a value of type {got}")
            },
            InstanceTypeError { class, field, got, expected, .. } => {
                write!(f, "Expected field '{field}' of class '{class}' to have type {expected}, but found type {got}")
            },
            CastError { err, .. } => write!(f, "Failed to cast top value on the stack: {err}"),
            ArrIdxOutOfBoundsError { got, max, .. } => write!(f, "Index {got} is out-of-bounds for an array of length {max}"),
            ProjUnknownFieldError { class, field, .. } => write!(f, "Class '{class}' has not field '{field}'"),
            VarDecError { err, .. } => write!(f, "Could not declare variable: {err}"),
            VarUndecError { err, .. } => write!(f, "Could not undeclare variable: {err}"),
            VarGetError { err, .. } => write!(f, "Could not get variable: {err}"),
            VarSetError { err, .. } => write!(f, "Could not set variable: {err}"),

            SpawnError { err, .. } => write!(f, "Failed to spawn new thread: {err}"),
            BranchTypeError { branch, got, expected, .. } => {
                write!(f, "Branch {branch} in parallel statement did not return value of type {expected}; got {got} instead")
            },
            IllegalBranchType { branch, merge, got, expected, .. } => write!(
                f,
                "Branch {branch} returned a value of type {got}, but the current merge strategy ({merge:?}) requires values of {expected} type"
            ),
            FunctionTypeError { name, arg, got, expected, .. } => {
                write!(f, "Argument {arg} for function '{name}' has incorrect type: expected {expected}, got {got}")
            },
            UnresolvedLocation { name, .. } => write!(f, "Cannot call task '{name}' because it has no resolved location."),
            UnknownInput { task, name, .. } => write!(f, "{} '{}' is not a possible input for task '{}'", name.variant(), name.name(), task),
            UnplannedInput { task, name, .. } => write!(f, "{} '{}' as input for task '{}' is not yet planned", name.variant(), name.name(), task),
            // UnavailableDataset{ name, .. }                        => write!(f, "Dataset '{}' is unavailable at execution time", name),
            FrameStackPushError { err, .. } => write!(f, "Failed to push to frame stack: {err}"),
            FrameStackPopError { err, .. } => write!(f, "Failed to pop from frame stack: {err}"),
            ReturnTypeError { got, expected, .. } => write!(f, "Got incorrect return type for function: expected {expected}, got {got}"),

            TaskTypeError { name, arg, got, expected, .. } => {
                write!(f, "Task '{name}' expected argument {arg} to be of type {expected}, but got {got}")
            },

            UnknownData { name, .. } => write!(f, "Encountered unknown dataset '{name}'"),
            UnknownResult { name, .. } => write!(f, "Encountered unknown result '{name}'"),
            UnknownPackage { name, version, .. } => write!(
                f,
                "Unknown package with name '{}'{}",
                name,
                if !version.is_latest() { format!(" and version {version}") } else { String::new() }
            ),
            ArgumentsSerializeError { err, .. } => write!(f, "Could not serialize task arguments: {err}"),

            StackError { err, .. } => write!(f, "{err}"),
            Custom { err, .. } => write!(f, "{err}"),
        }
    }
}

impl Error for VmError {}

/// Defines errors that occur only in the LocalVm.
#[derive(Debug)]
pub enum LocalVmError {
    /// Failed to Base64-decode a Task's response.
    Base64DecodeError { name: String, raw: String, err: base64::DecodeError },
    /// Failed to decode the given bytes as UTF-8.
    Utf8DecodeError { name: String, err: std::string::FromUtf8Error },
    /// Failed to decode the string as JSON.
    JsonDecodeError { name: String, raw: String, err: serde_json::Error },

    /// A given dataset was not found at the current location.
    DataNotAvailable { name: String, loc: String },
    /// The given data's path was not found.
    IllegalDataPath { name: String, path: PathBuf, err: std::io::Error },
    /// The given asset's path contained a colon.
    ColonInDataPath { name: String, path: PathBuf },
    /// The Transfer task is not supported by the LocalVm.
    TransferNotSupported,
}

impl Display for LocalVmError {
    #[inline]
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use LocalVmError::*;
        match self {
            Base64DecodeError { name, raw, err } => write!(f, "Could not decode result '{raw}' from task '{name}' as Base64: {err}"),
            Utf8DecodeError { name, err } => write!(f, "Could not decode base64-decoded result from task '{name}' as UTF-8: {err}"),
            JsonDecodeError { name, raw, err } => write!(f, "Could not decode result '{raw}' from task '{name}' as JSON: {err}"),

            DataNotAvailable { name, loc } => write!(f, "Dataset '{name}' is not available on the local location '{loc}'"),
            IllegalDataPath { name, path, err } => write!(f, "Invalid path '{}' to dataset '{}': {}", path.display(), name, err),
            ColonInDataPath { name, path } => {
                write!(f, "Encountered colon (:) in path '{}' to dataset '{}'; provide another path without", path.display(), name)
            },
            TransferNotSupported => write!(f, "Transfers are not supported in the LocalVm"),
        }
    }
}

impl Error for LocalVmError {}

/// Defines errors for the DummyVm.
#[derive(Debug)]
pub enum DummyVmError {
    /// Failed to run a workflow.
    ExecError { err: VmError },
}

impl Display for DummyVmError {
    fn fmt(&self, f: &mut Formatter<'_>) -> FResult {
        use DummyVmError::*;
        match self {
            ExecError { err } => write!(f, "Failed to execute workflow: {err}"),
        }
    }
}

impl Error for DummyVmError {}
