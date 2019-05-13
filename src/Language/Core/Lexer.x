{
module Language.Core.Lexer (
    Token(..),
    alexScanTokens,
) where

import Language.Core.Syntax
import Data.ByteString.Lazy (ByteString)
}

%wrapper "basic-bytestring"

$digit      = [0-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]
$special    = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
$graphic    = $printable # $white

@string     = \" ($graphic # \")* \"
@id         = [A-Za-z][A-Za-z'_]*
@escape     = '\\' ($printable | 'x' $hexdig+ | 'o' $octdig+ | $digit+)
@char       = ($graphic # $special) | @escape
@varid      = [a-z_][a-zA-Z0-9_'\#]*
@conid      = [A-Z][a-zA-Z0-9_'\#]*

tokens :-
    $white+     ;

    "--".*      { TokenLineComment }

    let         { \s -> TokenLet }
    case        { \s -> TokenCase }
    of          { \s -> TokenOf }
    join        { \s -> TokenJoin }

    "{"         { \s -> TokenLBrace }
    "}"         { \s -> TokenRBrace }
    "("         { \s -> TokenLParen }
    ")"         { \s -> TokenRParen }
    "(#"        { \s -> TokenLParenSharp }
    "#)"        { \s -> TokenRParenSharp }
    "["         { \s -> TokenLBracket }
    "]"         { \s -> TokenRBracket }
    "->"        { \s -> TokenArrow }
    "`"         { \s -> TokenBackQuote }
    "@"         { \s -> TokenAt }
    ":"         { \s -> TokenColon }
    "::"        { \s -> TokenDColon }
    "~"         { \s -> TokenTilde }

    @string       { TokenStrLit }
    @char         { TokenCharLit }
    @varid        { TokenVar }
    @conid        { TokenCon }

{

data Token
    = TokenLet
    | TokenCase
    | TokenOf
    | TokenJoin

    | TokenLBrace
    | TokenRBrace
    | TokenLParen
    | TokenRParen
    | TokenLParenSharp
    | TokenRParenSharp
    | TokenLBracket
    | TokenRBracket
    | TokenArrow
    | TokenBackQuote
    | TokenAt
    | TokenColon
    | TokenDColon
    | TokenTilde

    | TokenStrLit ByteString
    | TokenCharLit ByteString
    | TokenDigit ByteString
    | TokenCon ByteString
    | TokenVar ByteString
    | TokenLineComment ByteString
    deriving (Eq, Show)
}
