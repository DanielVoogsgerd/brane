//  AST NEW.rs
//    by Lut99
// 
//  Created:
//    06 Feb 2023, 10:19:28
//  Last edited:
//    06 Feb 2023, 15:25:59
//  Auto updated?
//    Yes
// 
//  Description:
//!   A second version of the AST that focusses less on being binary, but
//!   instead uses the fact that it's serialized using serde to retain its
//!   graph structure literally.
// 

use std::rc::Rc;


/***** LIBRARY *****/
/// The workflow represents a "binary", i.e., something fully executable start to finish.
pub struct Workflow {
    /// The bodies of the edges in this workflow. There is always at least one element, which is the main function (i.e., the first element is the main body).
    pub funcs : Vec<Rc<GraphElem>>,
    /// Contains the definitions in this workflow.
    pub defs  : DefTable,
}



/// The definition table represents the definitions and whatnot in a module that the VM cares about, but is also relevant information for linking against the module. Essentially a runtime symbol table.
pub struct DefTable {
    /// The tasks that can be executed in this workflow.
    pub tasks   : Vec<TaskDef>,
    /// The functions that can be executed in this workflow.
    pub funcs   : Vec<FuncDef>,
    /// The classes in this workflow.
    pub classes : Vec<ClassDef>,
}

/// Represents a task's definition.
pub struct TaskDef {
    
}

/// Represents a function's definition.
pub struct FuncDef {
    
}

/// Represents a class' definition (containing nested functions).
pub struct ClassDef {
    
}



/// The Module represents a single snippet, essentially. It is very similar to a workflow, except that the snippet may contain unresolved references that will be linked later.
pub struct Module {
    /// The bodies of the edges in this module. There is always at least one element, which is the main function.
    pub funcs : Vec<Rc<GraphElem>>,
    /// Contains the link (options) in this module.
    pub defs  : DefTable,
}



/// The linkage table is very similar to a DefTable, except that it may contain unresolved references as well.
pub struct LinkTable {
    /// The tasks that can be executed in this workflow.
    pub tasks   : Vec<Option<TaskDef>>,
    /// The functions that can be executed in this workflow.
    pub funcs   : Vec<Option<FuncDef>>,
    /// The classes in this workflow.
    pub classes : Vec<Option<ClassDef>>,
}



/// The GraphElem represent the parts of the graph. Can either be an `Edge`, to represent a transition, or a `Node`, which represents a state the reasoner cares about.
pub enum GraphElem {
    /// It's a state the reasoner cares about.
    Node(Node),
    /// It's a transition to some node that the reasoner cares about.
    Edge(Edge),
}

/// A Node represents a state the reasoner cares about. This is actually just a task.
pub struct Node {
    
}

/// An Edge represents a transition between nodes. These may contain nested instructions or otherwise, but nothing that the reasoner should care about. Cannot contain tasks.
pub struct Edge {
    /// Contains kind-specific information for the edge.
    pub kind   : EdgeVariant,
    /// The set of edge instructions that should be executed before the edge is taken.
    pub instrs : Vec<EdgeInstr>,
}

/// Defines information specific to each edge variant.
pub enum EdgeVariant {
    /// A Linear edge provides a linear connection from one graph element to another.
    Linear {
        /// The next graph element to move to.
        next : Rc<GraphElem>,
    },

    /// A branching edge allows the program to take one of two branches. Whichever one is taken is based on <TODO>.
    Branch {
        /// The element to move to if the condition is true.
        true_next  : Rc<GraphElem>,
        /// The element to move to if the condition is false.
        false_next : Rc<GraphElem>,
    },

    /// An iterate edge takes a given set of elements exactly the given amount of times, then continues with the second part.
    Iterate {
        /// The number of times to iterate.
        n    : usize,
        /// The graph element to take to loop.
        body : Rc<GraphElem>,
        /// The next element in the graph to do when the iteration completed.
        next : Rc<GraphElem>,
    },
    /// A repeat edge takes a given set of elements a runtime-dependent number of times before it continues with the second part. How many times is dependent on <TODO>.
    Repeat {
        /// The graph element to take to loop.
        body : Rc<GraphElem>,
        /// The next element in the graph to do when the repetition completed.
        next : Rc<GraphElem>,
    },
    /// A LoopEnd is a plug at the end of a loop body so that the execution engine knows when to go back to the start of the loop.
    LoopEnd,

    /// A parallel edge allows a given number of branches to be executed simultaneously. The edge returns a JoinHandle object, which represents the task itself.
    Parallel {
        /// The branches that must be run in parallel. Every branch is defined as a function.
        branches : Vec<Rc<GraphElem>>,
        /// The next graph element to move to once the branches have been _launched_ (not completed; see the Join-edge for that).
        next     : Rc<GraphElem>,
    },
    /// A join edge waits until all the branches in a given join handle have returned, then executes the next edge.
    Join {
        /// The next graph element to move to.
        next : Rc<GraphElem>,
    },

    /// A call edge represents a "function call" in workflow land. Contains the definition of the function to run. Note that this is only for local calls; external calls must be a `Node`.
    Call {
        /// The definition of the function to call.
        def : usize,
    },
    /// A return edge represents a "function return" in workflow land. Acts as a plug to a function body (and since the workflow body acts as a function, same effect).
    Return,
}



/// The EdgeInstr represents an instruction that can live within an Edge. Can be used to form a tiny binary program that operates on a stack machine.
pub enum EdgeInstr {
    
}
