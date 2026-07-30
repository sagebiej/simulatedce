## data.table's special symbols and the walrus used in := assignment cannot be
## seen by R CMD check. Everything else now goes through .data or a quoted name.
utils::globalVariables(c(".BY", ".SD", ".GRP", ":=", "est", "rob_pval0"))
