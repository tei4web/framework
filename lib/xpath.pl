:- module(xpath, []).
:- use_module(ebnf).

comma(X, Y, (Y, X)).

xpath(Xpath) --> whitespace, expr(Xpath).
expr(Expr) --> expr_single(Expr0), star(X, (lexeme(`,`), expr_single(X)), Exprs),
    { reverse(Expr, ExprR), foldl(comma, Exprs, Expr0, ExprR) }.

expr_single(Expr) --> for_expr(Expr);
    quantified_expr(Expr);
    if_expr(Expr);
    or_expr(Expr).

for_expr(for(Vars, Expr)) --> simple_for_clause(Vars), lexeme(`return`), expr_single(Expr).

simple_for_clause([V = Expr | Tail]) --> lexeme(`for`), lexeme(`$`),
    var_name(V), lexeme(`in`), expr_single(Expr),
    star(V0 = Expr0,
         (lexeme(`,`), lexeme(`$`), var_name(V0), lexeme(`in`), expr_single(Expr))).

% quantified_expr(Expr) --> lexeme((`some`; `every`)),
%     lexeme(`$`), var_name( "in" ExprSingle ("," "$" VarName "in" ExprSingle)* "satisfies" ExprSingle
    
% IfExpr --> "if" "(" Expr ")" "then" ExprSingle "else" ExprSingle
% OrExpr --> AndExpr ( "or" AndExpr )*
% AndExpr --> ComparisonExpr ( "and" ComparisonExpr )*
% ComparisonExpr --> RangeExpr ( (ValueComp
% | GeneralComp
% | NodeComp) RangeExpr )?
% RangeExpr --> AdditiveExpr ( "to" AdditiveExpr )?
% AdditiveExpr --> MultiplicativeExpr ( ("+" | "-") MultiplicativeExpr )*
% MultiplicativeExpr --> UnionExpr ( ("*" | "div" | "idiv" | "mod") UnionExpr )*
% UnionExpr --> IntersectExceptExpr ( ("union" | "|") IntersectExceptExpr )*
% IntersectExceptExpr --> InstanceofExpr ( ("intersect" | "except") InstanceofExpr )*
% InstanceofExpr --> TreatExpr ( "instance" "of" SequenceType )?
% TreatExpr --> CastableExpr ( "treat" "as" SequenceType )?
% CastableExpr --> CastExpr ( "castable" "as" SingleType )?
% CastExpr --> UnaryExpr ( "cast" "as" SingleType )?
% UnaryExpr --> ("-" | "+")* ValueExpr
% ValueExpr --> PathExpr
% GeneralComp --> "=" | "!=" | "<" | "<=" | ">" | ">="
% ValueComp --> "eq" | "ne" | "lt" | "le" | "gt" | "ge"
% NodeComp --> "is" | "<<" | ">>"
% PathExpr --> ("/" RelativePathExpr?)
% | ("//" RelativePathExpr)
% | RelativePathExpr /* xgs: leading-lone-slash */
% RelativePathExpr --> StepExpr (("/" | "//") StepExpr)*
% StepExpr --> FilterExpr | AxisStep
% AxisStep --> (ReverseStep | ForwardStep) PredicateList
% ForwardStep --> (ForwardAxis NodeTest) | AbbrevForwardStep
% ForwardAxis --> ("child" "::")
% | ("descendant" "::")
% | ("attribute" "::")
% | ("self" "::")
% | ("descendant-or-self" "::")
% | ("following-sibling" "::")
% | ("following" "::")
% | ("namespace" "::")
% AbbrevForwardStep --> "@"? NodeTest
% ReverseStep --> (ReverseAxis NodeTest) | AbbrevReverseStep
% ReverseAxis --> ("parent" "::")
% | ("ancestor" "::")
% | ("preceding-sibling" "::")
% | ("preceding" "::")
% | ("ancestor-or-self" "::")
% AbbrevReverseStep --> ".."
% NodeTest --> KindTest | NameTest
% NameTest --> QName | Wildcard
% Wildcard --> "*"
% | (NCName ":" "*")
% | ("*" ":" NCName) /* ws: explicit */
% FilterExpr --> PrimaryExpr PredicateList
% PredicateList --> Predicate*
% Predicate --> "[" Expr "]"
% PrimaryExpr --> Literal | VarRef | ParenthesizedExpr | ContextItemExpr | FunctionCall
% Literal --> NumericLiteral | StringLiteral
% NumericLiteral --> IntegerLiteral | DecimalLiteral | DoubleLiteral
% VarRef --> "$" VarName
% VarName --> QName
% ParenthesizedExpr --> "(" Expr? ")"
% ContextItemExpr --> "."
% FunctionCall --> QName "(" (ExprSingle ("," ExprSingle)*)? ")" /* xgs: reserved-function-names */
%  /* gn: parens */
% SingleType --> AtomicType "?"?
% SequenceType --> ("empty-sequence" "(" ")")
% | (ItemType OccurrenceIndicator?)
% OccurrenceIndicator --> "?" | "*" | "+" /* xgs: occurrence-indicators */
% ItemType --> KindTest | ("item" "(" ")") | AtomicType
% AtomicType --> QName
% KindTest --> DocumentTest
% | ElementTest
% | AttributeTest
% | SchemaElementTest
% | SchemaAttributeTest
% | PITest
% | CommentTest
% | TextTest
% | AnyKindTest
% AnyKindTest --> "node" "(" ")"
% DocumentTest --> "document-node" "(" (ElementTest | SchemaElementTest)? ")"
% TextTest --> "text" "(" ")"
% CommentTest --> "comment" "(" ")"
% PITest --> "processing-instruction" "(" (NCName | StringLiteral)? ")"
% AttributeTest --> "attribute" "(" (AttribNameOrWildcard ("," TypeName)?)? ")"
% AttribNameOrWildcard --> AttributeName | "*"
% SchemaAttributeTest --> "schema-attribute" "(" AttributeDeclaration ")"
% AttributeDeclaration --> AttributeName
% ElementTest --> "element" "(" (ElementNameOrWildcard ("," TypeName "?"?)?)? ")"
% ElementNameOrWildcard --> ElementName | "*"
% SchemaElementTest --> "schema-element" "(" ElementDeclaration ")"
% ElementDeclaration --> ElementName
% AttributeName --> QName
% ElementName --> QName
% TypeName --> QName

