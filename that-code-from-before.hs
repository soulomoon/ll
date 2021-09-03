Right
  (Decl { declName = "foo"
        , declRhs =
            Let [ Decl { declName = "x"
                       , declRhs =
                          Let
                            [ Decl {declName = "y", declRhs = Var "z", declWhere = Nothing} ]
                            (Var "y")
                       , declWhere = Nothing
                       }
                ]
                (Var "x")
        , declWhere = Nothing
        })