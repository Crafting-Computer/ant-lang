module AntEmitter exposing (emitDecls)

import AntParser exposing (Decl(..), Expr(..), Stmt(..))


emitDecls : List Decl -> String
emitDecls ds =
    String.join "\n" <|
        List.map emitDecl ds


emitDecl : Decl -> String
emitDecl d =
    case d of
        FnDecl f ->
            emitFunction f
        
        ImplDecl { target, functions } ->
            String.join "\n" <|
            List.map emitFunction functions
