module AntDebug exposing (main)

import AntParser
import AntChecker
import Html exposing (div, pre, text)


source =
--     """
-- struct MyStruct {
--     a : String,
--     b : MyStruct2,
-- }
-- struct MyStruct2 {
--     a : String,
--     b : Int,
-- }
-- fn createMyStruct(id : Int) : MyStruct {
--     MyStruct {
--         a = "abc",
--         b = if id > 0 {
--             MyStruct2 { a = "abc", b = 0 }
--         } else {
--             MyStruct2 { a = "abc", b = 1 }
--         }
--     }
-- }
-- """
--     """struct MyStruct {
--     a : Int
-- }
-- impl MyStruct {
--     fn increment(mut self : MyStruct) : () {
--         let self.a = self.a + 1;
--     }
-- }
-- fn foo(s : MyStruct) : MyStruct {
--     call MyStruct::increment(s);
--     s
-- }
-- """
--     """fn foo(a : Int, _ : Char) : Int {
--     var _ = a + 3;
--     var d = a - 2;
--     a
-- }
-- """
--     """
-- struct Person {
--     name : String,
--     age : Int
-- }

-- trait Show {
--     fn show(self : Self) : String;
--     fn changeName(self : Self, newName : String) : ();
-- }

-- impl Show for Person {
--     fn show(self : Self) : String {
--         "wala"
--     }
--     fn changeName(mut self: Self, newName : String) : () {
--         let self.name = newName;
--         call <Person as Show>::show(self);
--     }
-- }

--     """
--     """
-- fn identity(a : Int) : Int { a }
-- fn increment(mut a : Int) : () {
--     let a = a + 1;
-- }
-- fn foo() : () {
--     var a = 3;
--     call increment(identity(a));
-- }
    -- """
--     """
-- struct Integer {
--     value : Int
-- }
-- fn identity(a : Integer) : Integer { a }
-- fn increment(mut a : Integer) : () {
--     let a.value = a.value + 1;
-- }
-- fn foo() : () {
--     var mut a = Integer { value = 3 };
--     call increment(identity(a));
-- }
--     """
    """
struct Located<T : Display> {
    from : Int,
    to : Int,
    value : T
}
impl Located<T : Display> {
    fn length(self : Self) : Int {
        self.to - self.from
    }
    fn value(self : Self) : T {
        self.value
    }
    fn dummyLocated(value : T) : Located<T> {
        Located::<T> {
            from = -1,
            to = -1,
            value = value,
        }
    }
    fn distance<S>(self : Self, other : Located<S>) : Int {
        if self.to > other.to {
            self.from - other.to
        } else {
            other.from - self.to
        }
    }
}

trait Display {
    fn show(self : Self) : String;
}

impl Display for Located<T : Display> {
    fn show(self : Self) : String {
        T::show(self.value)
    }
}

fn identity<T>(a : T) : T { a }
fn foo() : Bool {
    var a = identity::Located::<String>(
        identity::Located::<String>(
            Located::<String> {
                from = 1,
                to = 6,
                value = "hello"
            }
        )
    );
    var b = Located::<Int> {
        from = 10,
        to = 11,
        value = 0,
    };
    Located::<String>::distance(a, b) == 4
}
    """


main =
    case AntParser.parseDecls source of
        Err err ->
            div []
                [ pre []
                    [ text <|
                        "❌ Parse error.\n\n"
                            ++ AntParser.showDeadEnds source err
                    ]
                ]

        Ok decls ->
            div []
                [ pre [] [ text "✔️ Passed parser." ]
                , pre [] [ text <| 
                    case AntChecker.checkDecls decls of
                        Ok _ ->
                            "✔️ Passed checker."
                        
                        Err problems ->
                            AntChecker.showProblems source problems
                ]
                ]
