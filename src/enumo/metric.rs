/// Measures for size of s-expressions
#[derive(Copy, PartialEq, Eq, Clone, Debug)]
pub enum Metric {
    /// Number of terminals in the s-expression
    Atoms,
    /// Number of lists in the s-expression
    /// This measures number of operators in the domain expression
    Lists,
    /// Depth the s-expression
    Depth,
}
