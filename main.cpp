#include <iostream>
#include <cstdarg>
#include <cstring>
#include <cassert>

#include "utility.h"
#include "array.h"
#include "our_string.h"



enum TokenType : u16 {
  TOKEN_IDENT        = 256,
  TOKEN_END_OF_INPUT = 257,

  TOKEN_SIGNED_NUMBER         = 258,
  TOKEN_FLOATING_POINT_NUMBER = 259,

  TOKEN_NUMBER  = 261,
  TOKEN_BOOLEAN = 260,

  TOKEN_IF_STATEMENT    = 400,
  TOKEN_ELSE_STATEMENT  = 401,
  TOKEN_FOR_STATEMENT   = 402,
  TOKEN_WHILE_STATEMENT = 403,

  TOKEN_DOUBLE_EQUALS     = 500,
  TOKEN_NOT_EQUALS        = 501,
  TOKEN_LOGICAL_OR        = 502,
  TOKEN_LOGICAL_AND       = 503,
  TOKEN_GREATER_OR_EQUALS = 515,
  TOKEN_LESS_OR_EQUALS    = 516,

  TOKEN_INCREMENT   = 504,
  TOKEN_DECREMENT   = 505,
  TOKEN_ADD_ASSIGN  = 506,
  TOKEN_SUB_ASSIGN  = 507,
  TOKEN_MUL_ASSIGN  = 508,
  TOKEN_DIV_ASSIGN  = 509,
  TOKEN_XOR_ASSIGN  = 510,
  TOKEN_AND_ASSIGN  = 511,
  TOKEN_OR_ASSIGN   = 512,
  TOKEN_RIGHT_SHIFT = 513,
  TOKEN_LEFT_SHIFT  = 514,

  TOKEN_SINGLE_CHARACTER = 515,
  TOKEN_STRING_LITERAL   = 516,

  TOKEN_STRUCT_DECLARATION = 517,

  TOKEN_ERROR = 666,
};

struct Var {
  TokenType type;
  union {
    bool bool_;
    s64  s64_;
    f64  f64_;
  };
};
/*

f64 from_var(Var v) {  // We don't know what return type this is going to be and putting `auto` here won't solve an issue ; compiler lies to us and always inferrs it as f64 . So `f64`.
  switch(v.type) {
    case TOKEN_SIGNED_NUMBER        : return v.s64_;
    case TOKEN_FLOATING_POINT_NUMBER: return v.f64_;
    case TOKEN_BOOLEAN              : return v.bool_;
  }
  assert(0);
  return 0;
}

s64 from_var_int_only(Var v) {
  assert(v.type != TOKEN_FLOATING_POINT_NUMBER);
  return from_var(v);
}

template<class BinaryOperator, class R>
Var do_binary_var_math(R (&getter)(Var), Var a, Var b) {
  BinaryOperator op;
  auto f1 = getter(a);
  auto f2 = getter(b);
  auto f3 = op(f1, f2);

  Var v;
  if(is_same<decltype(f3), f64>::value) {
    v.type = TOKEN_FLOATING_POINT_NUMBER;
    v.f64_ = f3;
  } else if(is_same<decltype(f3), s64>::value) {
    v.type = TOKEN_SIGNED_NUMBER;
    v.s64_ = f3;
  } else if(is_same<decltype(f3), bool>::value) {
    v.type  = TOKEN_BOOLEAN;
    v.bool_ = f3;
  } else {
    assert(0);
  }
  return v;
}

template<class UnaryOperator, class R>
Var do_unary_var_math(R (&getter)(Var), Var a) {
  UnaryOperator op;
  auto f1 = getter(a);
  auto f3 = op(f1);

  Var v;
  if(is_same<decltype(f3), f64>::value) {
    v.type = TOKEN_FLOATING_POINT_NUMBER;
    v.f64_ = f3;
  } else if(is_same<decltype(f3), s64>::value) {
    v.type = TOKEN_SIGNED_NUMBER;
    v.s64_ = f3;
  } else if(is_same<decltype(f3), bool>::value) {
    v.type  = TOKEN_BOOLEAN;
    v.bool_ = f3;
  } else {
    assert(0);
  }
  return v;
}

Var operator!(Var a) {  // @CrazyStuff: When I try to do it with functor, C++ decides to call from_var_int_only even though I'm passing from_var ; so @Copy&Paste: Fuck C++.
  auto f1 = from_var(a);
  auto f3 = !f1;
  Var v;
  if(is_same<decltype(f3), f64>::value) {
    v.type = TOKEN_FLOATING_POINT_NUMBER;
    v.f64_ = f3;
  } else if(is_same<decltype(f3), s64>::value) {
    v.type = TOKEN_SIGNED_NUMBER;
    v.s64_ = f3;
  } else if(is_same<decltype(f3), bool>::value) {
    v.type  = TOKEN_BOOLEAN;
    v.bool_ = f3;
  } else {
    assert(0);
  }
  return v;
}

Var operator*(Var a, Var b)  { return do_binary_var_math<multiply>  (from_var, a, b); }
Var operator/(Var a, Var b)  { return do_binary_var_math<divide>    (from_var, a, b); }
Var operator+(Var a, Var b)  { return do_binary_var_math<add>       (from_var, a, b); }
Var operator-(Var a, Var b)  { return do_binary_var_math<subtract>  (from_var, a, b); }
Var operator==(Var a, Var b) { return do_binary_var_math<equals>    (from_var, a, b); }
Var operator!=(Var a, Var b) { return do_binary_var_math<not_equals>(from_var, a, b); }
Var operator%(Var a, Var b)  { return do_binary_var_math<modulo>    (from_var_int_only, a, b); }
Var operator<(Var a, Var b)  { return do_binary_var_math<less>      (from_var, a, b); }
Var operator>(Var a, Var b)  { return do_binary_var_math<greater>   (from_var, a, b); }
Var operator<=(Var a, Var b) { return do_binary_var_math<less_or_equals>    (from_var, a, b); }
Var operator>=(Var a, Var b) { return do_binary_var_math<greater_or_equals> (from_var, a, b); }
Var operator&(Var a, Var b)  { return do_binary_var_math<bit_and> (from_var_int_only, a, b); }
Var operator|(Var a, Var b)  { return do_binary_var_math<bit_or>  (from_var_int_only, a, b); }
Var operator^(Var a, Var b)  { return do_binary_var_math<bit_xor> (from_var_int_only, a, b); }
Var operator&&(Var a, Var b) { return do_binary_var_math<logic_and> (from_var, a, b); }
Var operator||(Var a, Var b) { return do_binary_var_math<logic_or>  (from_var, a, b); }

Var operator+(Var a)         { return do_unary_var_math<plus>      (from_var, a); }
Var operator-(Var a)         { return do_unary_var_math<minus>     (from_var, a); }
Var operator~(Var a)         { return do_unary_var_math<bit_not>   (from_var_int_only, a); }
*/

struct Token {
  TokenType type = TOKEN_END_OF_INPUT;

  Var var;

  literal string_literal;
  s32 l, c;
};

struct Keyword_Def {
  literal   name;
  TokenType type;
};


struct Scope {
  array<literal>                names;
  array<struct Ast_Expression*> decls;
};

struct Block {
};

enum Ast_Type : u16 {
  Ast_VariableType = 0,
  Ast_IdentType,
  Ast_ProcCallType,
  Ast_LiteralType,
  Ast_PlusType,
  Ast_MinusType,
  Ast_LogicNotType,
  Ast_BitwiseNotType,
  Ast_AddType,
  Ast_SubType,
  Ast_MulType,
  Ast_DivType,
  Ast_ModType,
  Ast_EqualsType,
  Ast_NotEqualsType,
  Ast_LessType,
  Ast_GreaterType,
  Ast_LessOrEqualsType,
  Ast_GreaterOrEqualsType,
  Ast_BitwiseAndType,
  Ast_BitwiseOrType,
  Ast_BitwiseXorType,
  Ast_LogicAndType,
  Ast_LogicOrType,
  Ast_IfType,
  Ast_ForType,
  Ast_WhileType,
  Ast_ProcType,
  Ast_Variable_DeclarationType,
  Ast_Function_DeclarationType,
};

struct Ast_Expression {
  Ast_Type type;
};


#define DEFINE_DEFAULT_CONSTRUCTOR(ast_type, inherit_from) ast_type() : inherit_from() { type = ast_type##Type; }
struct Ast_Variable : public Ast_Expression {
  literal         name;
  Ast_Expression *expr = NULL;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Variable, Ast_Expression);
};

struct Ast_Ident : public Ast_Expression {
  literal name;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Ident, Ast_Expression);
};

struct Ast_ProcCall : public Ast_Expression {
  literal name;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_ProcCall, Ast_Expression);
};

struct Ast_Literal : public Ast_Expression {
  Var var;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Literal, Ast_Expression);
};

struct Ast_Declaration : public Ast_Expression {
  literal decl_type, decl_ident;
};

struct Ast_Variable_Declaration : public Ast_Declaration {
  Ast_Expression *expr = NULL;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Variable_Declaration, Ast_Declaration);
};

struct Ast_Function_Declaration : public Ast_Declaration {
  array<Ast_Expression*> arguments;
  array<Ast_Expression*> block;
  Scope scope, *parent;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Function_Declaration, Ast_Declaration);
};

struct Ast_BinaryOperator : public Ast_Expression {
  Ast_Expression *left  = NULL;
  Ast_Expression *right = NULL;
};

struct Ast_UnaryOperator : public Ast_Expression {
  Ast_Expression *left  = NULL;
};

struct Ast_Plus : public Ast_UnaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Plus, Ast_UnaryOperator);
};
struct Ast_Minus : public Ast_UnaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Minus, Ast_UnaryOperator);
};
struct Ast_LogicNot : public Ast_UnaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_LogicNot, Ast_UnaryOperator);
};
struct Ast_BitwiseNot : public Ast_UnaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_BitwiseNot, Ast_UnaryOperator);
};
struct Ast_Add : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Add, Ast_BinaryOperator);
};
struct Ast_Sub : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Sub, Ast_BinaryOperator);
};
struct Ast_Mul : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Mul, Ast_BinaryOperator);
};
struct Ast_Div : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Div, Ast_BinaryOperator);
};
struct Ast_Mod : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Mod, Ast_BinaryOperator);
};
struct Ast_Equals : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Equals, Ast_BinaryOperator);
};
struct Ast_NotEquals : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_NotEquals, Ast_BinaryOperator);
};
struct Ast_Less : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Less, Ast_BinaryOperator);
};
struct Ast_Greater : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Greater, Ast_BinaryOperator);
};
struct Ast_LessOrEquals : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_LessOrEquals, Ast_BinaryOperator);
};
struct Ast_GreaterOrEquals : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_GreaterOrEquals, Ast_BinaryOperator);
};
struct Ast_BitwiseAnd : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_BitwiseAnd, Ast_BinaryOperator);
};
struct Ast_BitwiseOr : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_BitwiseOr, Ast_BinaryOperator);
};
struct Ast_BitwiseXor : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_BitwiseXor, Ast_BinaryOperator);
};
struct Ast_LogicAnd : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_LogicAnd, Ast_BinaryOperator);
};
struct Ast_LogicOr : public Ast_BinaryOperator {
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_LogicOr, Ast_BinaryOperator);
};

struct Ast_If : public Ast_Expression {
  Ast_Expression *condition;

  Scope                  then_scope;
  array<Ast_Expression*> then_block;

  Scope                  else_scope;
  array<Ast_Expression*> else_block;

  array<Ast_Expression>         if_else_conditions;
  array<Scope>                  if_else_scopes;
  array<array<Ast_Expression*>> if_else_blocks;
    
  Scope *parent;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_If, Ast_Expression);
};


/*
// @Speed: Hash table!
struct Variables_Array {
  array<literal> names;
  array<Var>     vars;
};
static Variables_Array variables;

// @Speed: Hash table!
struct Global_Scope { // For functions, only?
  array<literal> names;
  array<Ast_Expression*> decls;
};
static Global_Scope global_scope;
*/

struct Ast_Proc : public Ast_Expression {
  using Ast_Expression::Ast_Expression;
  literal                name;
  array<Ast_Expression*> block;
  s32                    number_of_arguments;
  DEFINE_DEFAULT_CONSTRUCTOR(Ast_Proc, Ast_Expression);
};



static s32 nline = 1, nchar = 1;
static literal currently_processed_filename; // used in `report_error`.



static void report_error(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  fprintf(stderr , "%s:%i:%i: " red("error") ": ", currently_processed_filename.data, nline, nchar);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

static void exit_lexer_with_error(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  report_error(fmt, args);
  va_end(args);
}


static const Keyword_Def *maybe_get_keyword(const char *c) {
  // @Speed: Actually this must be a hashtable for O(1) lookup.
  static const Keyword_Def keyword_table[] = {
    {"if",    TOKEN_IF_STATEMENT},
    {"else",  TOKEN_ELSE_STATEMENT},
    {"for",   TOKEN_FOR_STATEMENT},
    {"while", TOKEN_WHILE_STATEMENT},
    {"struct", TOKEN_STRUCT_DECLARATION},
  };

  for(size_t i = 0; i < array_size(keyword_table); i++) {
    if(c == keyword_table[i].name) {
      ADVANCE(c, keyword_table[i].name.size);

      if(is_one_of(*c, " (\r\t\n")) {
        return &keyword_table[i];
      } else {
        continue;
      }
    }
  }
  return NULL;
}

enum comment_helper_t : u8 {
  uninitialized,
  single_line_comment,
  multi_line_comment,
};
static bool is_comment(const char *c, comment_helper_t *comment) {
  if(*c == '/') {
    INC(c);
    if(*c == '/') {
      *comment = single_line_comment;
    } else if(*c == '*') {
      *comment = multi_line_comment;
    } else {
      *comment = uninitialized;
    }
  } else {
    *comment = uninitialized;
  }
  return *comment != uninitialized;
}


static array<Token> tokens;
static s32 current_token_index = 0;

static Token *peek_token(s32 i = 0) { return &tokens[current_token_index + i]; }
static void   eat_token(s32 i = 0)  { current_token_index += i+1; }
static Token *peek_than_eat_token(s32 i = 0) {
  Token *tok = peek_token(i);
  eat_token(i);
  return tok;
}


static void lex(array<Token> *tokens, const char *cursor) {

  // @Speed: Hash tables for O(1) lookup.
  // For tokens search takes O(n).
  const char      single_symbol_tokens[]     = "(){}=;,.*&[]+-/!<>%?:#|^~";
  const literal   multiple_symbol_literals[] = {"==", "!=", "||", "&&", "++", "--", "+=", "-=", "*=", "/=", "^=", "&=", "|=", ">>", "<<", ">=", "<="};
  const TokenType multiple_symbol_tokens[]   = {TOKEN_DOUBLE_EQUALS, TOKEN_NOT_EQUALS, TOKEN_LOGICAL_OR, TOKEN_LOGICAL_AND, TOKEN_INCREMENT, TOKEN_DECREMENT, TOKEN_ADD_ASSIGN, TOKEN_SUB_ASSIGN, TOKEN_MUL_ASSIGN, TOKEN_DIV_ASSIGN, TOKEN_XOR_ASSIGN, TOKEN_AND_ASSIGN, TOKEN_OR_ASSIGN, TOKEN_RIGHT_SHIFT, TOKEN_LEFT_SHIFT, TOKEN_GREATER_OR_EQUALS, TOKEN_LESS_OR_EQUALS};
  size_t found_token_index; // while searching for multiple tokens we set `index` to indicate position of needed token in `multiple_symbol_tokens`.

  static_assert(array_size(multiple_symbol_literals) == array_size(multiple_symbol_tokens));

  comment_helper_t comment_type = uninitialized;


  while(*cursor != '\0') {
    Token tok;
    tok.l = nline;
    tok.c = nchar;

    if(const Keyword_Def *k = maybe_get_keyword(cursor)) {
      // Keyword token.
      ADVANCE(cursor, k->name.size);
      tok.string_literal = k->name;
      tok.type           = k->type;
      tokens->add(tok);
      //

#if 0
      @OMG:
    } else if(*cursor == '\"' || *cursor == '\'') {

      char f = *cursor;

      if(f == '\'') {
        INC(cursor);
        tok.type = TOKEN_SINGLE_CHARACTER;
        tok.string_literal = string(cursor, 1);
      } else {
      }
    }
#endif

    } else if(cursor == literal("true") || cursor == literal("false")) {
      // Bool token.
      tok.type = TOKEN_BOOLEAN;
      if(cursor[0] == 't') {
        assert(cursor == literal("true"));
        ADVANCE(cursor, 4);
        tok.var.type  = TOKEN_BOOLEAN;
        tok.var.bool_ = true;
        tok.string_literal = "true";

      } else if(cursor[0] == 'f') {
        assert(cursor == literal("false"));
        ADVANCE(cursor, 5);
        tok.var.type  = TOKEN_BOOLEAN;
        tok.var.bool_ = false;
        tok.string_literal = "false";

      } else {
        assert(0);
      }
      tokens->add(tok);
      // 
  
    } else if(isalpha(*cursor) || *cursor == '_') {
      // Identifier token.
      const char *tmp = cursor;
      size_t count = 1;
      INC(cursor);
      while(isalpha(*cursor) || *cursor == '_' || isdigit(*cursor)) {
        INC(cursor);
        count++;
      }

      tok.type           = TOKEN_IDENT;
      tok.string_literal = literal(tmp, count);
      tokens->add(tok);
      //

    } else if(isdigit(*cursor) || *cursor == '.') {
      // Number token.
      const char *tmp = cursor;

      size_t count = 0;
      bool is_floating_point_number = false;

      if(*cursor == '.') {
        is_floating_point_number = true;
        count++;
        INC(cursor);

        if(!isdigit(*cursor)) {
          exit_lexer_with_error("`.` is an operator. @Incomplete: Not handled yet!\n");
          tokens->add(tok);
        }
      } else {
        count++;
        INC(cursor);
      }

      while(1) {
        if(isdigit(*cursor)) {
          count++;
          INC(cursor);

        } else if(*cursor == '.') {
          if(is_floating_point_number) {
            exit_lexer_with_error("Too many decimal points in a number\n");
            tokens->add(tok);
          } else {
            is_floating_point_number = true;
          }
          count++;
          INC(cursor);

        } else {
          break;
        }
      }

      tok.type           = TOKEN_NUMBER;
      tok.string_literal = literal(tmp, count);

      if(is_floating_point_number) {
        tok.var.f64_ = atof(tmp);
        tok.var.type = TOKEN_FLOATING_POINT_NUMBER;
      } else {
        tok.var.s64_ = atoi(tmp);
        tok.var.type = TOKEN_SIGNED_NUMBER;
      }
      tokens->add(tok);
      //

    } else if(is_comment(cursor, &comment_type)) {
      // Comment.
      if(comment_type == single_line_comment) {
        assert(cursor == literal("//"));
        ADVANCE(cursor, 2, &nline, &nchar);

        while(*cursor != '\0') {
          if(*cursor == '\n') { break; }
          INC(cursor, &nline, &nchar);
        }
        INC(cursor, &nline, &nchar);

      } else {
        assert(comment_type == multi_line_comment && cursor == literal("/*"));
        ADVANCE(cursor, 2, &nline, &nchar);

        s32 depth = 1;
        while(*cursor != '\0') {
          if(depth == 0) { break; }

          if(*cursor == '*') {
            if(*cursor == '\0') { break; }
            INC(cursor, &nline, &nchar);
            if(*cursor == '/') { depth--; }

          } else if(*cursor == '/') {
            if(*cursor == '\0') { break; }
            INC(cursor, &nline, &nchar);
            if(*cursor == '*') { depth++; }
          }
          INC(cursor, &nline, &nchar);
        }
      }
      // 

    } else if(is_one_of(cursor, multiple_symbol_literals, &found_token_index)) {
      // Multiple symbols token.
      tok.type           = multiple_symbol_tokens[found_token_index];
      tok.string_literal = multiple_symbol_literals[found_token_index];
      ADVANCE(cursor, tok.string_literal.size);
      tokens->add(tok);
      //
  
    } else if(is_one_of(*cursor, single_symbol_tokens)) {
      // Single symbol token.
      tok.type           = (TokenType)(*cursor);
      tok.string_literal = literal(cursor, 1);
      INC(cursor);
      tokens->add(tok);
      //
    
    } else {
      INC(cursor, &nline, &nchar);
    }

    nchar += tok.string_literal.size;
  }

  assert(*cursor == '\0');
  Token tok;
  tok.type = TOKEN_END_OF_INPUT;
  tokens->add(tok);
}


#define NEW_AST(ast)    new ast
#define DELETE_AST(ast) delete ast

#define CASE_DELETE_JUST(ast_type) \
  case ast_type##Type: { \
    auto e = static_cast<ast_type*>(ast); \
    DELETE_AST(e); \
    break; \
  }
#define CASE_DELETE_BINARY_OPERATOR(ast_type) \
  case ast_type##Type: { \
    auto e = static_cast<ast_type*>(ast); \
    dealloc(e->left); \
    dealloc(e->right); \
    DELETE_AST(e); \
    break; \
  }
#define CASE_DELETE_AST(ast_type, member) \
  case ast_type##Type: { \
    auto e = static_cast<ast_type*>(ast); \
    dealloc(e->member); \
    DELETE_AST(e); \
    break; \
  }
    

static void dealloc(Ast_Expression *ast) {
  if(!ast) return;

  switch(ast->type) {
    CASE_DELETE_JUST(Ast_Literal);
    CASE_DELETE_JUST(Ast_Ident);
    CASE_DELETE_BINARY_OPERATOR(Ast_Add);
    CASE_DELETE_BINARY_OPERATOR(Ast_Sub);
    CASE_DELETE_BINARY_OPERATOR(Ast_Mul);
    CASE_DELETE_BINARY_OPERATOR(Ast_Div);
    CASE_DELETE_BINARY_OPERATOR(Ast_Mod);
    CASE_DELETE_BINARY_OPERATOR(Ast_Equals);
    CASE_DELETE_BINARY_OPERATOR(Ast_NotEquals);
    CASE_DELETE_BINARY_OPERATOR(Ast_Less);
    CASE_DELETE_BINARY_OPERATOR(Ast_Greater);
    CASE_DELETE_BINARY_OPERATOR(Ast_LessOrEquals);
    CASE_DELETE_BINARY_OPERATOR(Ast_GreaterOrEquals);
    CASE_DELETE_BINARY_OPERATOR(Ast_BitwiseAnd);
    CASE_DELETE_BINARY_OPERATOR(Ast_BitwiseOr);
    CASE_DELETE_BINARY_OPERATOR(Ast_BitwiseXor);
    CASE_DELETE_BINARY_OPERATOR(Ast_LogicAnd);
    CASE_DELETE_BINARY_OPERATOR(Ast_LogicOr);

    CASE_DELETE_AST(Ast_Plus, left);
    CASE_DELETE_AST(Ast_Minus, left);
    CASE_DELETE_AST(Ast_LogicNot, left);
    CASE_DELETE_AST(Ast_BitwiseNot, left);

    CASE_DELETE_JUST(Ast_If); // @Incomplete: @RemoveMe:
    CASE_DELETE_AST(Ast_Variable, expr); // @RemoveMe: 
  }
  ast = NULL;
}


static void exit_parser_with_error(Ast_Expression *a, const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  if(fmt) { vfprintf(stderr, fmt, args); }
  va_end(args);
  dealloc(a);
}

static bool check_token(const Token *tok, TokenType t, Ast_Expression *a, const char *fmt, ...) {
  if(tok->type != t) {
    va_list args;
    exit_parser_with_error(a, fmt, args);
    return false;
  }
  return true;
}

static bool check_ast(const Ast_Expression *ast, Ast_Expression *a, const char *fmt, ...) {
  if(!ast) {
    va_list args;
    exit_parser_with_error(a, fmt, args);
    return false;
  }
  return true;
}



static Ast_Expression *parse_binary_operator(Ast_Expression *, s8);
static Ast_Expression *parse_expression(s8);
static void parse(Scope *, array<Ast_Expression*> *, TokenType);

static bool is_unary_operator(TokenType t) {
  switch(t) {
    case '+':
    case '-':
    case '~':
    case '!':
      return true;
    default: 
      return false;
  }
}

static bool is_binary_operator(TokenType t) {
  switch(t) {
    case '*':
    case '/':
    case '%':
    case '+':
    case '-':
    case TOKEN_RIGHT_SHIFT:
    case TOKEN_LEFT_SHIFT :
    case '>':
    case '<':
    case TOKEN_GREATER_OR_EQUALS:
    case TOKEN_LESS_OR_EQUALS:
    case TOKEN_DOUBLE_EQUALS:
    case TOKEN_NOT_EQUALS:
    case '&':
    case '|':
    case '^':
    case TOKEN_LOGICAL_AND:
    case TOKEN_LOGICAL_OR:
      return true;
    default: 
      return false;
  }
}

static s8 get_unop_precedence(TokenType t) {
  switch(t) {
    case '~':
    case '!':
    case '+':
    case '-':
      return 15;
    defualt: assert(0); return 0;
  }
}

static s8 get_binop_precedence(TokenType t) {
  switch(t) {
    case '*':
    case '/':
    case '%':
      return 13;
    case '+':
    case '-':
      return 12;
    case TOKEN_RIGHT_SHIFT:
    case TOKEN_LEFT_SHIFT :
      return 11;
    case '>':
    case '<':
    case TOKEN_GREATER_OR_EQUALS:
    case TOKEN_LESS_OR_EQUALS:
      return 9;
    case TOKEN_DOUBLE_EQUALS:
    case TOKEN_NOT_EQUALS:
      return 8;
    case '&':
      return 7;
    case '^':
      return 6;
    case '|':
      return 5;
    case TOKEN_LOGICAL_AND:
      return 4;
    case TOKEN_LOGICAL_OR:
      return 3;
    defualt: assert(0); return 0;
  }
}

static Ast_Type ast_unop_type_from_token_type(TokenType t) {
  Ast_Type e = (Ast_Type)-1;
  switch(t) {
    case '~':                     return Ast_BitwiseNotType;
    case '!':                     return Ast_LogicNotType;
    case '+':                     return Ast_PlusType;
    case '-':                     return Ast_MinusType;
    default:                      assert(0); return e;
  }
}

static Ast_Type ast_binop_type_from_token_type(TokenType t) {
  Ast_Type e = (Ast_Type)-1;
  switch(t) {
    case '*':                     return Ast_MulType;
    case '/':                     return Ast_DivType;
    case '%':                     return Ast_ModType;
    case '+':                     return Ast_AddType;
    case '-':                     return Ast_SubType;
    case TOKEN_RIGHT_SHIFT:       return e;
    case TOKEN_LEFT_SHIFT :       return e;
    case '>':                     return Ast_GreaterType;
    case '<':                     return Ast_LessType;
    case TOKEN_GREATER_OR_EQUALS: return Ast_GreaterOrEqualsType;
    case TOKEN_LESS_OR_EQUALS:    return Ast_LessOrEqualsType;
    case TOKEN_DOUBLE_EQUALS:     return Ast_EqualsType;
    case TOKEN_NOT_EQUALS:        return Ast_NotEqualsType;
    case '&':                     return Ast_BitwiseAndType;
    case '|':                     return Ast_BitwiseOrType;
    case '^':                     return Ast_BitwiseXorType;
    case TOKEN_LOGICAL_AND:       return Ast_LogicAndType;
    case TOKEN_LOGICAL_OR:        return Ast_LogicOrType;
    default:                      assert(0); return e;
  }
}


static Ast_Expression *parse_binary_operator(Ast_Expression *left, s8 priority) {
  const Token    *tok  = peek_token(-1);
  const TokenType type = tok->type;

  if (is_binary_operator(type)) {
    s8 p = get_binop_precedence(type);
    if(priority < p) {
      auto op   = NEW_AST(Ast_BinaryOperator);
      op->type  = ast_binop_type_from_token_type(type);
      op->left  = left;
      op->right = parse_expression(p);
      return parse_binary_operator(op, priority);
    } else {
      return left;
    }

  } else if (is_one_of(type, ";{)")) {
    return left;

  } else { 
    // @Panic:
    get_string_from_literal(name, tok->string_literal);
    report_error("unhandled parse_binary_operator() switch :%s:%d:\n", name, type); 
  }
  assert(0);
  return NULL;
}

static const s8 MIN_PRIORITY = -1;
static Ast_Expression *parse_expression(s8 priority = MIN_PRIORITY) {
  const Token    *tok  = peek_than_eat_token();
  const TokenType type = tok->type;

  Ast_Expression *left = NULL;
  if (is_unary_operator(type)) {
    s8     p = get_unop_precedence(type);
    s8 new_p = (priority < p) ? p : MIN_PRIORITY;

    auto op = NEW_AST(Ast_UnaryOperator);
    op->type = ast_unop_type_from_token_type(type);
    op->left = parse_expression(new_p);
    left     = op;
    return parse_binary_operator(left, priority);


  } else if (type == TOKEN_IDENT) {
    auto lit = tok->string_literal;

    if(peek_token()->type == '(') { // It's a function call.
      eat_token();
      assert(peek_token()->type == ')'); // @Incomplete: for now we handle only 0 args.
      eat_token();

      auto e  = NEW_AST(Ast_ProcCall);
      e->name = lit;
      left    = e;

    } else { // assuming it's just a variable.
      auto e = NEW_AST(Ast_Ident);
      e->name = lit;
      left    = e;
    }

  } else if (type == TOKEN_NUMBER || type == TOKEN_BOOLEAN) {
    auto e = NEW_AST(Ast_Literal);
    e->var = tok->var;
    left   = e;

  } else if (type == '(') {
    left = parse_expression();

  } else {
    get_string_from_literal(name, tok->string_literal);
    report_error("unhandled parse_expression() switch :%s:%d:\n", name, type); 
    assert(0);
  }

  eat_token();
  return parse_binary_operator(left, priority);
}

static void parse_if_statement(Ast_If *if_, Ast_Expression *condition, Scope *then_scope, array<Ast_Expression*> *then_block, Scope *else_scope, array<Ast_Expression*> *else_block) {
  if(peek_than_eat_token()->type != '(') { assert(0); } // @ReportError.
  condition = parse_expression();
  assert(peek_token(-1)->type == ')'); // AssertWillGetFired: We actually don't know which token terminated the parse_expression(). So we need to pass it in parse_expression as an argument. Then this assert is fine.
  if (!condition) { return; } // Error must have already been reported.

  if(peek_than_eat_token()->type != '{') { assert(0); } // @ReportError.

  parse(then_scope, then_block, (TokenType)'}');

  Token *tok = peek_token();
  if(tok->type == TOKEN_ELSE_STATEMENT) {
    eat_token();
    tok = peek_than_eat_token();
    if(tok->type == TOKEN_IF_STATEMENT) {
      // } else if(...) {
      auto new_condition = if_->if_else_conditions.add();
      auto new_scope     = if_->if_else_scopes.add();
      auto new_block     = if_->if_else_blocks.add();
      parse_if_statement(if_, &new_condition, &new_scope, &new_block, else_scope, else_block);
      assert(peek_token(-1)->type == '}');
      // Done.

    } else if(tok->type == '{') {
      // } else {
      parse(else_scope, else_block, (TokenType)'}');
      // Done.
      
    } else {
      assert(0); // @ReportError.
    }
  } else {
    // Done.
  }
}

static void parse(Scope *current_scope, array<Ast_Expression*> *block = NULL, TokenType terminator = TOKEN_END_OF_INPUT) {
  while(1) {
    literal         ident;
    Ast_Expression *ast = NULL;

    Token *tok = peek_than_eat_token();
    if(tok->type == TOKEN_IDENT) {
      // int ...;
      literal decl_type, decl_ident;

      decl_type  = tok->string_literal;
      tok        = peek_than_eat_token(); 
      if(tok->type != TOKEN_IDENT) { assert(0); }  // @ReportError.
      decl_ident = tok->string_literal;
      
      tok = peek_than_eat_token();
      if(tok->type == '(') {
        // Function declaration.
        auto fdecl = NEW_AST(Ast_Function_Declaration);
        fdecl->decl_type  = decl_type;
        fdecl->decl_ident = decl_ident;
        fdecl->parent     = current_scope;
        ident             = decl_ident;
        ast               = fdecl;

        // parse argument list. @Incomplete:
        //

        if(peek_than_eat_token()->type != ')') { assert(0); } // @ReportError.
        if(peek_than_eat_token()->type != '{') { assert(0); } // @ReportError.

        // parse block. @Incomplete:
        parse(&fdecl->scope, &fdecl->block, (TokenType)'}');

        assert(peek_token(-1)->type == '}');

      } else if(tok->type == '=') {
        // Variable declaration.
        auto vdecl = NEW_AST(Ast_Variable_Declaration);
        vdecl->decl_type  = decl_type;
        vdecl->decl_ident = decl_ident;
        vdecl->expr       = parse_expression();
        ident             = decl_ident;
        ast               = vdecl;

      } else {
        assert(0); // @ReportError.
      }


    } else if(tok->type == TOKEN_STRUCT_DECLARATION) {
      // struct Name { ... };


    } else if(tok->type == terminator) {
      // End of program.
      ast = NULL;

    } else if(block && tok->type == TOKEN_IF_STATEMENT) {

      auto if_ = NEW_AST(Ast_If);
      if_->parent = current_scope;
      parse_if_statement(if_, if_->condition, &if_->then_scope, &if_->then_block, &if_->else_scope, &if_->else_block);
      block->add(if_);
      continue;


    } else {
      get_string_from_literal(name, tok->string_literal); 
      report_error("Unhandled tok := %s : %d\n", name, tok->type); // @ReportError:
      assert(0);
    }

    if(ast) {
      // Parsed AST node correctly.
      current_scope->names.add(ident);
      current_scope->decls.add(ast);

    } else {
      break;
    }
  }
}

/*
#define CASE_DO_BINOP(binop, ast_type) \
  case ast_type##Type: { \
    const auto a = static_cast<const ast_type*>(expr); \
    return interp_expr(a->left) binop interp_expr(a->right); \
  }
#define CASE_DO_UNOP(unop, ast_type) \
  case ast_type##Type: { \
    const auto a = static_cast<const ast_type*>(expr); \
    return unop interp_expr(a->left); \
  }

static Var interp_expr(Ast_Expression *expr) {
  switch(expr->type) {
    CASE_DO_UNOP(+, Ast_Plus);
    CASE_DO_UNOP(-, Ast_Minus);
    CASE_DO_UNOP(!, Ast_LogicNot);
    CASE_DO_UNOP(~, Ast_BitwiseNot); // @WrongTypes:

    CASE_DO_BINOP(*, Ast_Mul);
    CASE_DO_BINOP(/, Ast_Div); // @ZeroDivision:
    CASE_DO_BINOP(+, Ast_Add);
    CASE_DO_BINOP(-, Ast_Sub);
    CASE_DO_BINOP(%, Ast_Mod); // @ZeroDivision: @WrongTypes:
    CASE_DO_BINOP(==, Ast_Equals);
    CASE_DO_BINOP(!=, Ast_NotEquals);
    CASE_DO_BINOP(<, Ast_Less);
    CASE_DO_BINOP(>, Ast_Greater);
    CASE_DO_BINOP(<=, Ast_LessOrEquals);
    CASE_DO_BINOP(>=, Ast_GreaterOrEquals);
    CASE_DO_BINOP(&, Ast_BitwiseAnd); // @WrongTypes: passing floating point number to bitwise ops will crash programm, however it's not OK.
    CASE_DO_BINOP(|, Ast_BitwiseOr);  // @WrongTypes:
    CASE_DO_BINOP(^, Ast_BitwiseXor); // @WrongTypes:
    CASE_DO_BINOP(&&, Ast_LogicAnd);
    CASE_DO_BINOP(||, Ast_LogicOr);

    case Ast_VariableType: {
      auto a = static_cast<Ast_Variable*>(expr);
      literal *iter; s32 index;
      variables.names.find(a->name, &iter, &index);
      assert(iter);
      return variables.vars[index];
    }

    case Ast_LiteralType:
      return static_cast<const Ast_Literal*>(expr)->var;

    case Ast_IdentType: {
      const auto a = static_cast<const Ast_Ident*>(expr);

      literal *iter; s32 index;
      variables.names.find(a->name, &iter, &index);
      if(iter) {
        return variables.vars[index];
      } else {
        // @Panic:
        report_error("Use of variable before define!\n");
        assert(0); // @Incomplete:
        Var v;
        return v;
      }
    }

    case Ast_ProcCallType: {
      const auto a = static_cast<const Ast_ProcCall*>(expr);

      literal *iter; s32 index;
      global_scope.names.find(a->name, &iter, &index);
      if(iter) {
        auto proc = static_cast<Ast_Proc *>(global_scope.decls[index]);

        for(int i = 0; i < proc->block.size-1; i++) {
          auto expr = proc->block[i];
          interp(expr);
        }
        return interp_expr(proc->block[proc->block.size-1]);

      } else {
        // @Panic:
        report_error("Use of function before define!\n");
        assert(0); // @Incomplete:
        Var v;
        return v;
      }
    }

    default: report_error("interp_expr() : unhandled switch case\n"); break;
  }
}

static void interp(Ast_Expression *ast) {
  switch(ast->type) {
    case Ast_VariableType: {
      const auto a = static_cast<const Ast_Variable*>(ast);

      literal *iter; s32 index;
      variables.names.find(a->name, &iter, &index);
      if(iter) {
        variables.vars[index] = interp_expr(a->expr);
      } else {

        // We should execute interp_expr before adding something to `variables.names`, cause now we have Ast_Vairable in interp_expr,
        // that means we should keep `names` and `vars` same sizes. @Temporary: 
        auto name = a->name;
        auto expr = interp_expr(a->expr);
        variables.names.add(name);
        variables.vars.add(expr);
      }
      break;
    }

    case Ast_ProcType : {
      auto a = static_cast<Ast_Proc*>(ast);
      global_scope.names.add(a->name);
      global_scope.decls.add(a);
      break;
    }

    case Ast_ForType  :
    case Ast_WhileType:
    case Ast_IfType   :
      break;

    default: report_error("interp() : unhandled switch case @ShouldNeverHappen:\n"); assert(0); break;
  }
}
*/

static void read_entire_file(string *r, FILE *f) {
  fseek(f, 0, SEEK_END);
  size_t size = ftell(f);

  r->resize(size);

  rewind(f);
  size_t res = fread(r->data, sizeof(char), size, f);
  if(res != size) { fprintf(stderr, "@Incomplete\n"); }
}

Ast_Declaration *get_decl_by_name(literal name, Scope *scope) {
  literal *iter; s32 index;
  scope->names.find(name, &iter, &index);
  if(iter) {
    return static_cast<Ast_Declaration*>(scope->decls[index]);
  } else {
    assert(0); // @NotHandledYet:
  }
}

s32 main(s32 argc, s8 **argv) {
  string content;

  if(argc > 1) {
    currently_processed_filename = literal(argv[1], strlen(argv[1]));

    FILE *file = fopen(argv[1], "r");
    defer { fclose(file); };
    read_entire_file(&content, file);

  } else {
    currently_processed_filename = "(null)";
    report_error("Not enough positional arguments passed!\n");
  }


  lex(&tokens, content.data);


  // @Preprocessor: ???


  Scope global_scope;
  parse(&global_scope);


  Ast_Function_Declaration *decl = (Ast_Function_Declaration*) get_decl_by_name("main", &global_scope);

  for(auto &global_decls : global_scope.names) { print(global_decls); }
  puts("**********");
  for(auto &main_decls   : decl->scope.names)  { print(main_decls); }


  Ast_If *a = (Ast_If*) decl->block[0];

  return 0;
}
