#include <iostream>
#include <string>
#include <fstream>
#include <cstdarg>
#include <cstring>
#include <cassert>

#include "utility.h"
#include "array.h"


enum TokenType : s16 {
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

  TOKEN_PROC_DEFINITION = 515,

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

f64 from_var(Var v) {  // We don't know what type this is going to be and putting `auto` here won't solve an issue ; compiler lies to us and always inferrs it as f64 . So `f64`.
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
Var do_var_math(R (&getter)(Var), Var a, Var b) {
  Var v;
  auto f1 = getter(a);
  auto f2 = getter(b);

  BinaryOperator op;
  auto f3 = op(f1, f2);
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

Var operator*(Var a, Var b)  { return do_var_math<multiply>  (from_var, a, b); }
Var operator/(Var a, Var b)  { return do_var_math<divide>    (from_var, a, b); }
Var operator+(Var a, Var b)  { return do_var_math<add>       (from_var, a, b); }
Var operator-(Var a, Var b)  { return do_var_math<subtract>  (from_var, a, b); }
Var operator==(Var a, Var b) { return do_var_math<equals>    (from_var, a, b); }
Var operator!=(Var a, Var b) { return do_var_math<not_equals>(from_var, a, b); }
Var operator%(Var a, Var b)  { return do_var_math<modulo>    (from_var_int_only, a, b); }
Var operator<(Var a, Var b)  { return do_var_math<less>      (from_var, a, b); }
Var operator>(Var a, Var b)  { return do_var_math<greater>   (from_var, a, b); }
Var operator<=(Var a, Var b)  { return do_var_math<less_or_equals>    (from_var, a, b); }
Var operator>=(Var a, Var b)  { return do_var_math<greater_or_equals> (from_var, a, b); }
Var operator&(Var a, Var b)  { return do_var_math<bit_and> (from_var_int_only, a, b); }
Var operator|(Var a, Var b)  { return do_var_math<bit_or>  (from_var_int_only, a, b); }
Var operator^(Var a, Var b)  { return do_var_math<bit_xor> (from_var_int_only, a, b); }


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


enum AstType : s16 {
  Ast_VariableType,
  Ast_LiteralType,
  Ast_MulType,
  Ast_DivType,
  Ast_AddType,
  Ast_SubType,
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
  Ast_LogicalAndType,
  Ast_LogicalOrType,
  Ast_IfType,
  Ast_ForType,
  Ast_WhileType,
  Ast_ProcType,
};

struct Ast_Expression {
  AstType type;
  Ast_Expression(AstType t) { type = t; }
};

struct Ast_Variable : public Ast_Expression {
  using Ast_Expression::Ast_Expression;
  literal         name;
  Ast_Expression *expr = NULL;
};

struct Ast_Literal : public Ast_Expression {
  using Ast_Expression::Ast_Expression;
  Var var;
};

struct Ast_BinaryOperator : public Ast_Expression {
  using Ast_Expression::Ast_Expression;
  Ast_Expression *left  = NULL;
  Ast_Expression *right = NULL;
};

struct Ast_Add : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Sub : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Mul : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Div : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Mod : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Equals : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_NotEquals : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Less : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_Greater : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_LessOrEquals : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_GreaterOrEquals : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_BitwiseAnd : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_BitwiseOr : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_BitwiseXor : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_LogicalAnd : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};
struct Ast_LogicalOr : public Ast_BinaryOperator {
  using Ast_BinaryOperator::Ast_BinaryOperator;
};

struct Ast_If : public Ast_Expression {
  using Ast_Expression::Ast_Expression;
  Ast_Expression *condition = NULL;
  //array<Ast_Expression *> then_block;
  //array<Ast_Expression *> else_block;
};


// @Speed: Hash table!
struct VariablesArray {
  array<literal> names;
  array<Var>     vars;
};
static VariablesArray variables;

struct Ast_Proc : public Ast_Expression {
  using Ast_Expression::Ast_Expression;
  literal        name;
  array<Ast_Expression*> block;
};



static s32 nline = 1, nchar = 1;
static const char *cursor = NULL;
static literal currently_processed_filename; // used in `report_error`.

static Token   current_token; // @Speed: if we already have this as global, there is no need to copy token inside get_next_token() twice! (first on current_token , second while returning). I say that get_next_token() and peek_next_token() must return a pointer to `current_token` or `peeked`.

static void report_error(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  get_string_from_literal(filename, currently_processed_filename);
  fprintf(stderr , "%s:%i:%i: " red("error") ": ", (const char *)filename, nline, nchar);
  vfprintf(stderr, fmt, args);
  va_end(args);
}

static void exit_lexer_with_error(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  current_token.type = TOKEN_ERROR;
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
    {"proc",  TOKEN_PROC_DEFINITION}
  };

  for(size_t i = 0; i < array_size(keyword_table); i++) {
    if(c == keyword_table[i].name) {
      ADVANCE(c, keyword_table[i].name.size);

      if(is_one_of(*c, " \r\t\n")) {
        return &keyword_table[i];
      } else {
        continue;
      }
    }
  }
  return NULL;
}

enum comment_helper_t {
  uninitialized,
  single_line_comment,
  multi_line_comment,
};

static comment_helper_t comment_type = uninitialized;
static comment_helper_t is_comment(const char *c) {
  if(*c == '/') {
    INC(c);
    if(*c == '/') {
      comment_type = single_line_comment;
    } else if(*c == '*') {
      comment_type = multi_line_comment;
    } else {
      comment_type = uninitialized;
    }
  } else {
    comment_type = uninitialized;
  }
  return comment_type;
}



static bool   needs_new_token = true; // 3 following variables are used for peek_next_token().
static size_t distance_to_next_token;
static Token  peeked;



static Token get_next_token() {
  Token tok;

  // @Speed: Hash tables for O(1) lookup.
  // For tokens search takes O(n).
  static const char      single_symbol_tokens[]     = "(){}=;,.*&[]+-/!<>%?:#|^~";
  static const literal   multiple_symbol_literals[] = {"==", "!=", "||", "&&", "++", "--", "+=", "-=", "*=", "/=", "^=", "&=", "|=", ">>", "<<", ">=", "<="};
  static const TokenType multiple_symbol_tokens[]   = {TOKEN_DOUBLE_EQUALS, TOKEN_NOT_EQUALS, TOKEN_LOGICAL_OR, TOKEN_LOGICAL_AND, TOKEN_INCREMENT, TOKEN_DECREMENT, TOKEN_ADD_ASSIGN, TOKEN_SUB_ASSIGN, TOKEN_MUL_ASSIGN, TOKEN_DIV_ASSIGN, TOKEN_XOR_ASSIGN, TOKEN_AND_ASSIGN, TOKEN_OR_ASSIGN, TOKEN_RIGHT_SHIFT, TOKEN_LEFT_SHIFT, TOKEN_GREATER_OR_EQUALS, TOKEN_LESS_OR_EQUALS};
  static size_t found_token_index; // while searching for multiple tokens we set `index` to indicate position of needed token in `multiple_symbol_tokens`.

  static_assert(array_size(multiple_symbol_literals) == array_size(multiple_symbol_tokens));



  if(!needs_new_token) {
    // We already know next token because we called peek_next_token().
    needs_new_token = true;
    ADVANCE(cursor, distance_to_next_token);
    return peeked;
  }


  defer {
    tok.l = nline;
    tok.c = nchar;

    // @Incomplete:
    // Multi line strings?
    nchar += tok.string_literal.size;
    current_token = tok;
  };

  while(*cursor != '\0') {
    if(const Keyword_Def *k = maybe_get_keyword(cursor)) {
      // Keyword token.
      ADVANCE(cursor, k->name.size);
      tok.string_literal = k->name;
      tok.type           = k->type;
      return tok;
      //

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
      return tok;
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
      return tok;
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
          return tok;
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
            return tok;
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
      return tok;
      //

    } else if(uninitialized != is_comment(cursor)) {
      // Comment.
      if(comment_type == single_line_comment) {
        assert(cursor == literal("//"));
        ADVANCE(cursor, 2);

        while(*cursor != '\0') {
          if(*cursor == '\n') { break; }
          INC(cursor);
        }
        INC(cursor);

      } else {
        assert(comment_type == multi_line_comment && cursor == literal("/*"));
        ADVANCE(cursor, 2);

        s32 depth = 1;
        while(*cursor != '\0') {
          if(depth == 0) { break; }

          if(*cursor == '*') {
            if(*cursor == '\0') { break; }
            INC(cursor);
            if(*cursor == '/') { depth--; }

          } else if(*cursor == '/') {
            if(*cursor == '\0') { break; }
            INC(cursor);
            if(*cursor == '*') { depth++; }
          }
          INC(cursor);
        }
      }
      // 

    } else if(is_one_of(cursor, multiple_symbol_literals, &found_token_index)) {
      // Multiple symbols token.
      tok.type           = multiple_symbol_tokens[found_token_index];
      tok.string_literal = multiple_symbol_literals[found_token_index];
      ADVANCE(cursor, tok.string_literal.size);
      return tok;
      //
  
    } else if(is_one_of(*cursor, single_symbol_tokens)) {
      // Single symbol token.
      tok.type           = (TokenType)(*cursor);
      tok.string_literal = literal(cursor, 1);
      INC(cursor);
      return tok;
      //
    
    } else {
      if(*cursor == '\n') {
        nline++;
        nchar = 1;
      } else {
        nchar++;
      }
      INC(cursor);
    }
  }

  tok.type = TOKEN_END_OF_INPUT;
  assert(*cursor == '\0');
  return tok;
}

static Token peek_next_token() {
  if(needs_new_token) {
    const char *tmp = cursor;
    peeked = get_next_token();
    distance_to_next_token = cursor - tmp;
    cursor = tmp;
    needs_new_token = false;
  }
  return peeked;
}

#define NEW_AST(ast)    new ast(ast##Type)
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
    CASE_DELETE_BINARY_OPERATOR(Ast_LogicalAnd);
    CASE_DELETE_BINARY_OPERATOR(Ast_LogicalOr);

    CASE_DELETE_JUST(Ast_If); // @Incomplete: @RemoveMe:
    CASE_DELETE_AST(Ast_Variable, expr); // @RemoveMe: 
  }
  ast = NULL;
}


static void exit_parser_with_error(Ast_Expression *a, const char *fmt, ...) {
  va_list args;
  if(fmt) { fprintf(stderr, fmt, args); }
  dealloc(a);
}
static void exit_parser(Ast_Expression *a) { exit_parser_with_error(a, NULL); }



static Ast_Expression *parse_binary_operator(Ast_Expression *, s8);
static Ast_Expression *parse_expression(s8);
static Ast_Expression *parse();
static Var             interp_expr(Ast_Expression*);
static void            interp(Ast_Expression*);


static void skip_block() {
  assert(current_token.type == '{'); // @NotJustAssert: needs to be a check producing an error.
  s32 depth = 1;
  while(depth != 0) { // Skipping `else` block.
    get_next_token();
    if(current_token.type == '{') { depth++; }
    if(current_token.type == '}') { depth--; }
  }
  assert(current_token.type == '}'); // @NotJustAssert:
}

static void interp_block() {
  assert(current_token.type == '{'); // @NotJustAssert:
  Token tok = peek_next_token();
  while(tok.type != '}') {
    interp(parse());
    tok = peek_next_token();
  }
  get_next_token();
  assert(current_token.type == '}'); // @NotJustAssert:
}

static bool found_else_block() {
  Token tok = peek_next_token();
  if(tok.type == TOKEN_ELSE_STATEMENT) {
    get_next_token();
    tok = get_next_token();
    if(tok.type != '{') {
      report_error("Expected '{' after else keyword\n"); // @Incomplete:
      return false;
    }
    return true;
  }
  return false;
}


template<TokenType T> s8 get_precedence();
template<> s8 get_precedence<(TokenType)'~'>() { return 15; }
template<> s8 get_precedence<(TokenType)'!'>() { return 15; }

template<> s8 get_precedence<(TokenType)'*'>() { return 13; }
template<> s8 get_precedence<(TokenType)'/'>() { return 13; }
template<> s8 get_precedence<(TokenType)'%'>() { return 13; }

template<> s8 get_precedence<TOKEN_RIGHT_SHIFT>() { return 11; }
template<> s8 get_precedence<TOKEN_LEFT_SHIFT>()  { return 11; }

template<> s8 get_precedence<(TokenType)'+'>() { return 12; }
template<> s8 get_precedence<(TokenType)'-'>() { return 12; }

template<> s8 get_precedence<(TokenType)'>'>()          { return 9; }
template<> s8 get_precedence<(TokenType)'<'>()          { return 9; }
template<> s8 get_precedence<TOKEN_GREATER_OR_EQUALS>() { return 9; }
template<> s8 get_precedence<TOKEN_LESS_OR_EQUALS>()    { return 9; }

template<> s8 get_precedence<TOKEN_DOUBLE_EQUALS>() { return 8; } 
template<> s8 get_precedence<TOKEN_NOT_EQUALS>()    { return 8; } 

template<> s8 get_precedence<(TokenType)'&'>() { return 7; }
template<> s8 get_precedence<(TokenType)'^'>() { return 6; }
template<> s8 get_precedence<(TokenType)'|'>() { return 5; } 

template<> s8 get_precedence<TOKEN_LOGICAL_AND>() { return 4; } 
template<> s8 get_precedence<TOKEN_LOGICAL_OR>()  { return 3; } 


#define CASE_PARSE_BINOP(token_type, ast_type) \
    case (TokenType)token_type: { \
      s8 p = get_precedence<(TokenType)token_type>(); \
      if(priority < p) { \
        auto op = NEW_AST(ast_type); \
        op->left  = left; \
        op->right = parse_expression(p); \
        return parse_binary_operator(op, priority); \
      } else { \
        return left; \
      } \
    }

static Ast_Expression *parse_binary_operator(Ast_Expression *left, s8 priority) {
  switch(current_token.type) {
    CASE_PARSE_BINOP('+', Ast_Add);
    CASE_PARSE_BINOP('-', Ast_Sub);
    CASE_PARSE_BINOP('*', Ast_Mul);
    CASE_PARSE_BINOP('/', Ast_Div);
    CASE_PARSE_BINOP('%', Ast_Mod);
    CASE_PARSE_BINOP(TOKEN_DOUBLE_EQUALS, Ast_Equals);
    CASE_PARSE_BINOP(TOKEN_NOT_EQUALS,    Ast_NotEquals);
    CASE_PARSE_BINOP('<',    Ast_Less);
    CASE_PARSE_BINOP('>',    Ast_Greater);
    CASE_PARSE_BINOP(TOKEN_LESS_OR_EQUALS,    Ast_LessOrEquals);
    CASE_PARSE_BINOP(TOKEN_GREATER_OR_EQUALS, Ast_GreaterOrEquals);
    CASE_PARSE_BINOP('&', Ast_BitwiseAnd);
    CASE_PARSE_BINOP('|', Ast_BitwiseOr);
    CASE_PARSE_BINOP('^', Ast_BitwiseXor);

    case ';':
    case '{': 
    case ')':
      return left;
      break;

    default : {
      get_string_from_literal(name, current_token.string_literal);
      report_error("unhandled parse_binary_operator() switch :%s:%d:\n", name, current_token.type); 
      break;
    }
  }
  
  assert(0);
  return NULL;
}

static Ast_Expression *parse_expression(s8 priority=-1) {
  Token tok = get_next_token();

  Ast_Expression *left = NULL;
  switch(tok.type) {
    case TOKEN_IDENT: {
      auto e = NEW_AST(Ast_Literal); // @CleanUp: can't be just Ast_Literal, it needs to have a name for interpretation phase.

      // @CleanUp: Need to do this on interpretation phase, not now.
      literal *iter; size_t index;
      variables.names.find(&iter, &index, tok.string_literal);
      if(iter) {
        e->var = variables.vars[index];
      } else {
        // @Incomplete: Check if this actually frees all the memory being used.
        exit_parser_with_error(e, "Use before define!\n");
        return NULL;
      }
      // 

      left = e;
      break;
    }
    case TOKEN_BOOLEAN:
    case TOKEN_NUMBER : {
      auto e = NEW_AST(Ast_Literal);
      e->var = tok.var;
      left   = e;
      break;
    }

    case '(': {
      left = parse_expression(-1);
      break;
    }

    case ';': {
      break;
    }

    case TOKEN_END_OF_INPUT: {
      return NULL;
    }

    default : {
      get_string_from_literal(name, tok.string_literal);
      report_error("unhandled parse_expression() switch :%s:%d:\n", name, tok.type); 
      break;
    }
  }

  tok = get_next_token();
  return parse_binary_operator(left, priority);
}

static Ast_Expression *parse() {
  Token tok = get_next_token();
  switch(tok.type) {
    case TOKEN_IDENT: {
      auto ast = NEW_AST(Ast_Variable);
      ast->name = tok.string_literal;

      tok = get_next_token();
      if(tok.type != '=') {
        exit_parser_with_error(ast, "Expected '=' in a variable definintion\n");
        return NULL;
      }

      ast->expr = parse_expression();
      if(!ast->expr) {
        exit_parser_with_error(ast, "OOPSIE! @Incomplete: \n");
        return NULL;
      }

      interp(ast);
      return ast;
    }

    case TOKEN_PROC_DEFINITION: {
      auto ast  = NEW_AST(Ast_Proc);

      Token tok = get_next_token();
      if(tok.type != TOKEN_IDENT) {
        exit_parser_with_error(ast, "Expected identifier after proc keyword.\n");
        return NULL;
      }

      tok = get_next_token();
      if(tok.type != '(') {
        exit_parser_with_error(ast, "Expected '(' after identifier in procedure definition.\n");
        return NULL;
      }

      size_t count = 0;
      while(tok.type != ')') {
        defer { count++; };

        tok = get_next_token();
        if(tok.type != TOKEN_IDENT) {
          exit_parser_with_error(ast, "Expected identifier inside of argument list\n");
          return NULL;
        }

        tok = get_next_token();
        if(tok.type == ')')      { break; }
        else if(tok.type != ',') {
          exit_parser_with_error(ast, "Expected ',' after an argument\n");
          return NULL;
        }
      }

      print("Got ", count, " arguments");
      tok = get_next_token();
      if(tok.type != '{') {
        exit_parser_with_error(ast, "Expected '{' after arguments list\n");
        return NULL;
      }

      // @Incomplete:
      tok = get_next_token();
      if(tok.type != '}') {
        exit_parser_with_error(ast, "Expected '}' after procedure block\n");
        return NULL;
      }
      // 

      return ast;
    }

    case TOKEN_IF_STATEMENT: {
      auto ast = NEW_AST(Ast_If);

      ast->condition = parse_expression();
      if(!ast->condition) {
        assert(current_token.type == TOKEN_ERROR);
        exit_parser(ast);
        return NULL;
      }

      auto condition = from_var(interp_expr(ast->condition));
      if(condition) {
        // parse and interp `then` block.
        interp_block();
        if(found_else_block()) { 
          skip_block();
        }

      } else { 
        // parse and interp `else` block
        skip_block();
        if(found_else_block()) {
          interp_block();
        }
      }
      return ast;
    }

    case TOKEN_FOR_STATEMENT: {
      break;
    }

    case TOKEN_WHILE_STATEMENT: {
      break;
    }

    case TOKEN_BOOLEAN: {
      tok = get_next_token();
      if(tok.type != ';') {
        exit_parser_with_error(NULL, "Expected ';' after boolean constant.\n");
        return NULL;
      }
      return parse();
    }

    case TOKEN_NUMBER: {
      tok = get_next_token();
      if(tok.type != ';') {
        exit_parser_with_error(NULL, "Expected ';' after numeric constant.\n");
        return NULL;
      }
      return parse();
    }

    case TOKEN_END_OF_INPUT:
      return NULL;

    default: {
      get_string_from_literal(name, tok.string_literal);
      report_error("Unhandled switch in `parse()` :%s:%d: \n", name, tok.type);
      assert(0);
      return NULL;
    }
  }
  assert(0);
  return NULL;
}

#define CASE_DO_BINOP(binop, ast_type) \
  case ast_type##Type: { \
    auto a = static_cast<ast_type*>(expr); \
    return interp_expr(a->left) binop interp_expr(a->right); \
  }

static Var interp_expr(Ast_Expression *expr) {
  switch(expr->type) {
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

    case Ast_LiteralType:
      return static_cast<Ast_Literal*>(expr)->var;

    default: report_error("interp_expr() : unhandled switch case\n"); break;
  }
}

static void interp(Ast_Expression *ast) {
  switch(ast->type) {
    case Ast_VariableType: {
      auto a = static_cast<Ast_Variable*>(ast);

      literal *iter; size_t index;
      variables.names.find(&iter, &index, a->name);
      if(iter) {
        variables.vars[index] = interp_expr(a->expr);
      } else {
        variables.names.add(a->name);
        variables.vars.add(interp_expr(a->expr));
      }
      break;
    }

    case Ast_ForType  :
    case Ast_WhileType:
    case Ast_IfType   :
      break;

    default: report_error("interp() : unhandled switch case\n"); break;
  }
}


s32 main(s32 argc, s8 **argv) {
  std::string content;

  if(argc > 1) {
    currently_processed_filename = literal(argv[1], strlen(argv[1]));
    std::ifstream f(argv[1]);
    std::string s;
    while(std::getline(f, s)) { content.append(s).push_back('\n'); }

  } else {
    currently_processed_filename = literal("(null)");
    report_error("Not enough positional arguments passed!\n");
  }

  cursor = content.c_str();

  while(Ast_Expression *ast = parse()) {
    interp(ast);
    dealloc(ast);
  }

  for(size_t i = 0; i < variables.names.size; i++) {
    auto name = variables.names[i];
    auto var  = variables.vars[i];
    print(name, " := ", from_var(var));
  }
  return 0;
}

