import Prelude

import Data.Either (Either (..))
import Data.Maybe (Maybe (..))

import Data.Bool (Bool (..))
import Language.PureScript
import Language.PureScript.AST.Declarations.ChainId (ChainId (..))
import Language.PureScript.Externs
import Language.PureScript.Label

-- import Language.PureScript.CST.Types
-- import Language.PureScript.Externs
-- import Language.PureScript.AST

ext1 =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "A"
        , efExports =
            [ ValueRef
                ( SourceSpan
                    { spanName = "tests/purs/make/A.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 8}
                    }
                )
                (Ident "foo")
            ]
        , efImports =
            [ ExternsImport
                { eiModule = ModuleName "Prim"
                , eiImportType = Implicit
                , eiImportedAs = Just (ModuleName "Prim")
                }
            , ExternsImport
                { eiModule = ModuleName "Prim"
                , eiImportType = Implicit
                , eiImportedAs = Nothing
                }
            ]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    TypeConstructor
                        ( SourceSpan
                            { spanName = ""
                            , spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                            , spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                            }
                        , []
                        )
                        ( Qualified
                            (ByModuleName (ModuleName "Prim"))
                            (ProperName {runProperName = "Int"})
                        )
                }
            ]
        , efSourceSpan =
            SourceSpan
                { spanName = "tests/purs/make/A.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 8}
                }
        }
ext2 =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "A"
        , efExports =
            [ ValueRef
                ( SourceSpan
                    { spanName =
                        "tests/purs/make/A.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 8}
                    }
                )
                (Ident "foo")
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    TypeConstructor
                        ( SourceSpan
                            { spanName = ""
                            , spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                            , spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                            }
                        , []
                        )
                        (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))
                }
            ]
        , efSourceSpan =
            SourceSpan
                { spanName =
                    "tests/purs/make/A.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 8}
                }
        }

-- module B (module E) where\nimport A (foo) as E\n
externsWithReexport =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "B"
        , efExports =
            [ ReExportRef
                ( SourceSpan
                    { spanName = "tests/purs/make/B.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 20}
                    }
                )
                ( ExportSource
                    { exportSourceImportedFrom = Just (ModuleName "A")
                    , exportSourceDefinedIn = ModuleName "A"
                    }
                )
                ( ValueRef
                    ( SourceSpan
                        { spanName = "tests/purs/make/B.purs"
                        , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                        , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 20}
                        }
                    )
                    (Ident "foo")
                )
            , ModuleRef
                ( SourceSpan
                    { spanName = "tests/purs/make/B.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 11}
                    , spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 19}
                    }
                )
                (ModuleName "E")
            ]
        , efImports =
            [ ExternsImport
                { eiModule = ModuleName "Prim"
                , eiImportType = Implicit
                , eiImportedAs = Just (ModuleName "Prim")
                }
            , ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}
            , ExternsImport {eiModule = ModuleName "A", eiImportType = Explicit [ValueRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}}) (Ident "foo")], eiImportedAs = Just (ModuleName "E")}
            ]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations = []
        , efSourceSpan =
            SourceSpan
                { spanName = "tests/purs/make/B.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 20}
                }
        }

-- "module A where\nfoo::forall par. par -> par\nfoo wow = wow\n"
-- contains "par" name that if change will change the type
externsWithTypeParams =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "A"
        , efExports =
            [ ValueRef
                ( SourceSpan
                    { spanName =
                        "tests/purs/make/A.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 14}
                    }
                )
                (Ident "foo")
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    ForAll
                        ( SourceSpan
                            { spanName = "tests/purs/make/A.purs"
                            , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 6}
                            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}
                            }
                        , []
                        )
                        "par"
                        ( Just
                            ( TypeConstructor
                                ( SourceSpan
                                    { spanName = "tests/purs/make/A.purs"
                                    , spanStart =
                                        SourcePos {sourcePosLine = 2, sourcePosColumn = 22}
                                    , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 24}
                                    }
                                , []
                                )
                                (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
                            )
                        )
                        (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 24}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 21}}, []) "par")) (TypeVar (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) "par"))
                        (Just (SkolemScope {runSkolemScope = 0}))
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 14}}
        }

-- module A (foo) where\ntype Foo=Int \nfoo::Foo\nfoo = 1\n
externsWithTypeSyn =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "A"
        , efExports = [ValueRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 8}}) (Ident "foo")]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    TypeConstructor
                        ( SourceSpan
                            { spanName =
                                "tests/purs/make/A.purs"
                            , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 10}
                            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 13}
                            }
                        , []
                        )
                        ( Qualified
                            (ByModuleName (ModuleName "Prim"))
                            ( ProperName
                                { runProperName = "Int"
                                }
                            )
                        )
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 8}}
        }

externsWithTypeSynRec =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "A"
        , efExports =
            [ ValueRef
                ( SourceSpan
                    { spanName = "tests/purs/make/A.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 12}
                    }
                )
                (Ident "foo")
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    TypeApp
                        ( SourceSpan
                            { spanName = "tests/purs/make/A.purs"
                            , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 10}
                            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 19}
                            }
                        , []
                        )
                        (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 10}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"})))
                        ( RCons
                            ( SourceSpan
                                { spanName =
                                    "tests/purs/make/A.purs"
                                , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}
                                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}
                                }
                            , []
                            )
                            (Label {runLabel = "x"})
                            ( TypeConstructor
                                (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}, [])
                                ( Qualified
                                    (ByModuleName (ModuleName "Prim"))
                                    ( ProperName {runProperName = "Int"}
                                    )
                                )
                            )
                            (KindApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 19}}, []) (REmpty (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 19}}, [])) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        )
                }
            ]
        , efSourceSpan =
            SourceSpan
                { spanName =
                    "tests/purs/make/A.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 12}
                }
        }

-- "module A (foo, bar) where\ntype Nest={x::Int}\ntype Foo={nest::Nest}\nfoo::Foo\nfoo ={nest:{x:1}}\nbar={nest:{x:2}}\n"
externsWithNestedRecType =
    ExternsFile
        { efVersion = "0.15.7"
        , efModuleName = ModuleName "A"
        , efExports = [ValueRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 6, sourcePosColumn = 17}}) (Ident "foo"), ValueRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 6, sourcePosColumn = 17}}) (Ident "bar")]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    TypeApp
                        ( SourceSpan
                            { spanName =
                                "tests/purs/make/A.purs"
                            , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 10}
                            , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}
                            }
                        , []
                        )
                        ( TypeConstructor
                            ( SourceSpan
                                { spanName = "tests/purs/make/A.purs"
                                , spanStart =
                                    SourcePos
                                        { sourcePosLine =
                                            3
                                        , sourcePosColumn = 10
                                        }
                                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 11}
                                }
                            , []
                            )
                            (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))
                        )
                        ( RCons
                            ( SourceSpan
                                { spanName = "tests/purs/make/A.purs"
                                , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 11}
                                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 21}
                                }
                            , []
                            )
                            (Label {runLabel = "nest"})
                            (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 19}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 12}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))) (RCons (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 12}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}, []) (Label {runLabel = "x"}) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))) (KindApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 19}}, []) (REmpty (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 19}}, [])) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))))
                            (KindApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 21}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}, []) (REmpty (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 21}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}, [])) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 17}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 21}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        )
                }
            , EDValue
                { edValueName = Ident "bar"
                , edValueType =
                    TypeApp
                        ( SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}
                        , []
                        )
                        ( TypeConstructor
                            ( SourceSpan
                                { spanName = ""
                                , spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                                , spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                                }
                            , []
                            )
                            (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))
                        )
                        ( RCons
                            ( SourceSpan
                                { spanName = ""
                                , spanStart =
                                    SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                                , spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                                }
                            , []
                            )
                            (Label {runLabel = "nest"})
                            ( TypeApp
                                ( SourceSpan
                                    { spanName = ""
                                    , spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                                    , spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}
                                    }
                                , []
                                )
                                (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"})))
                                (RCons (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Label {runLabel = "x"}) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))) (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))))
                            )
                            (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        )
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 6, sourcePosColumn = 17}}
        }

-- module A where\nclass CsA a where mA :: a -> Int\n
externsJustTypeClass =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports =
            [ ValueRef
                (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}})
                (Ident "mA")
            , TypeClassRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}) (ProperName {runProperName = "CsA"})
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "mA"
                , edValueType =
                    ForAll (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a" (Just (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))) (ConstrainedType (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Constraint {constraintAnn = (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []), constraintClass = Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "CsA"}), constraintKindArgs = [], constraintArgs = [TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a"], constraintData = Nothing}) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}, []) "a")) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 30}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))))) (Just (SkolemScope {runSkolemScope = 0}))
                }
            , EDType
                { edTypeName = ProperName {runProperName = "CsA"}
                , edTypeKind =
                    TypeApp
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( TypeConstructor
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            ( Qualified
                                (ByModuleName (ModuleName "Prim"))
                                (ProperName {runProperName = "Constraint"})
                            )
                        )
                , edTypeDeclarationKind = ExternData [Nominal]
                }
            , EDType
                { edTypeName = ProperName {runProperName = "CsA$Dict"}
                , edTypeKind = TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))
                , edTypeDeclarationKind =
                    DataType
                        Newtype
                        [
                            ( "a"
                            , Just (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))
                            , Representational
                            )
                        ]
                        [(ProperName {runProperName = "CsA$Dict"}, [TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))) (RCons (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Label {runLabel = "mA"}) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}, []) "a")) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 30}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))) (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))))])]
                }
            , EDDataConstructor
                { edDataCtorName =
                    ProperName {runProperName = "CsA$Dict"}
                , edDataCtorOrigin = Newtype
                , edDataCtorTypeCtor = ProperName {runProperName = "CsA$Dict"}
                , edDataCtorType = ForAll (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a" (Just (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))) (RCons (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Label {runLabel = "mA"}) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}, []) "a")) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 30}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))) (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "CsA$Dict"}))) (TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a"))) Nothing
                , edDataCtorFields = [Ident "dict"]
                }
            , EDClass
                { edClassName = ProperName {runProperName = "CsA"}
                , edClassTypeArguments = [("a", Just (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))]
                , edClassMembers = [(Ident "mA", TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 27}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 29}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 25}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}, []) "a")) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 30}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))))]
                , edClassConstraints = []
                , edFunctionalDependencies = []
                , edIsEmpty = False
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 33}}
        }

{-
module B where
import A
class CsA a <= CsB a where mB :: a -> Int
instance CsB Int where mB i = i
 -}
externsWithTCInherited =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "B"
        , efExports =
            [ ValueRef
                (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 32}})
                (Ident "mB")
            , TypeClassRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 32}}) (ProperName {runProperName = "CsB"})
            , TypeInstanceRef
                (SourceSpan {spanName = "<generated>", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}})
                (Ident "csBInt")
                CompilerNamed
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "A", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "mB"
                , edValueType =
                    ForAll
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        "a"
                        (Just (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( ConstrainedType
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            ( Constraint
                                { constraintAnn =
                                    (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                , constraintClass = Qualified (ByModuleName (ModuleName "B")) (ProperName {runProperName = "CsB"})
                                , constraintKindArgs = []
                                , constraintArgs = [TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a"]
                                , constraintData = Nothing
                                }
                            )
                            ( TypeApp
                                ( SourceSpan
                                    { spanName = "tests/purs/make/B.purs"
                                    , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}
                                    , spanEnd =
                                        SourcePos
                                            { sourcePosLine = 3
                                            , sourcePosColumn = 42
                                            }
                                    }
                                , []
                                )
                                (TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 35}}, []) "a"))
                                (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
                            )
                        )
                        (Just (SkolemScope {runSkolemScope = 0}))
                }
            , EDType
                { edTypeName =
                    ProperName
                        { runProperName = "CsB"
                        }
                , edTypeKind =
                    TypeApp
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( TypeConstructor
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            ( Qualified
                                (ByModuleName (ModuleName "Prim"))
                                (ProperName {runProperName = "Constraint"})
                            )
                        )
                , edTypeDeclarationKind = ExternData [Nominal]
                }
            , EDType
                { edTypeName = ProperName {runProperName = "CsB$Dict"}
                , edTypeKind = TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))
                , edTypeDeclarationKind =
                    DataType
                        Newtype
                        [("a", Just (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))), Representational)]
                        [
                            ( ProperName {runProperName = "CsB$Dict"}
                            ,
                                [ TypeApp
                                    (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                    (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"})))
                                    ( RCons
                                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                        (Label {runLabel = "mB"})
                                        ( TypeApp
                                            ( SourceSpan
                                                { spanName = "tests/purs/make/B.purs"
                                                , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}
                                                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}
                                                }
                                            , []
                                            )
                                            (TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 35}}, []) "a"))
                                            (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
                                        )
                                        ( RCons
                                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                            (Label {runLabel = "CsA0"})
                                            (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))) (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "CsA$Dict"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}, []) "a")))
                                            ( KindApp
                                                (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                                (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []))
                                                ( TypeConstructor
                                                    ( SourceSpan
                                                        { spanName = "tests/purs/make/B.purs"
                                                        , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}
                                                        , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}
                                                        }
                                                    , []
                                                    )
                                                    (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
                                                )
                                            )
                                        )
                                    )
                                ]
                            )
                        ]
                }
            , EDDataConstructor
                { edDataCtorName = ProperName {runProperName = "CsB$Dict"}
                , edDataCtorOrigin = Newtype
                , edDataCtorTypeCtor = ProperName {runProperName = "CsB$Dict"}
                , edDataCtorType =
                    ForAll
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        "a"
                        (Just (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( TypeApp
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            ( TypeApp
                                (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"})))
                                ( TypeApp
                                    (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                    (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"})))
                                    ( RCons
                                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                        (Label {runLabel = "mB"})
                                        (TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 35}}, []) "a")) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))))
                                        (RCons (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Label {runLabel = "CsA0"}) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Record"}))) (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "CsA$Dict"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}, []) "a"))) (KindApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (REmpty (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))))
                                    )
                                )
                            )
                            (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "B")) (ProperName {runProperName = "CsB$Dict"}))) (TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a"))
                        )
                        Nothing
                , edDataCtorFields = [Ident "dict"]
                }
            , EDClass
                { edClassName = ProperName {runProperName = "CsB"}
                , edClassTypeArguments =
                    [
                        ( "a"
                        , Just
                            ( TypeConstructor
                                ( SourceSpan
                                    { spanName = "tests/purs/make/B.purs"
                                    , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}
                                    , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}
                                    }
                                , []
                                )
                                (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
                            )
                        )
                    ]
                , edClassMembers =
                    [
                        ( Ident "mB"
                        , TypeApp
                            ( SourceSpan
                                { spanName =
                                    "tests/purs/make/B.purs"
                                , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}
                                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}
                                }
                            , []
                            )
                            (TypeApp (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 35}}, []) "a"))
                            (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))
                        )
                    ]
                , edClassConstraints =
                    [ Constraint
                        { constraintAnn =
                            ( SourceSpan
                                { spanName = "tests/purs/make/B.purs"
                                , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}
                                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}
                                }
                            , []
                            )
                        , constraintClass = Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "CsA"})
                        , constraintKindArgs = []
                        , constraintArgs = [TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}, []) "a"]
                        , constraintData = Nothing
                        }
                    ]
                , edFunctionalDependencies = []
                , edIsEmpty = False
                }
            , EDInstance
                { edInstanceClassName = Qualified (ByModuleName (ModuleName "B")) (ProperName {runProperName = "CsB"})
                , edInstanceName = Ident "csBInt"
                , edInstanceForAll = []
                , edInstanceKinds = []
                , edInstanceTypes =
                    [ TypeConstructor
                        (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 4, sourcePosColumn = 14}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 17}}, [])
                        ( Qualified
                            (ByModuleName (ModuleName "Prim"))
                            (ProperName {runProperName = "Int"})
                        )
                    ]
                , edInstanceConstraints = Just []
                , -- , edInstanceChain = Just (ChainId ("tests/purs/make/B.purs", SourcePos {sourcePosLine = 4, sourcePosColumn = 1}))
                  edInstanceChainIndex = 0
                , edInstanceNameSource = UserNamed
                , edInstanceSourceSpan = SourceSpan {spanName = "<generated>", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 32}}
        }

-- fn x = mB x (mb is member of typeclass)
funUsesTypeClass =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "C"
        , efExports =
            [ ValueRef
                (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}})
                (Ident "fn")
            ]
        , efImports =
            [ ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}
            , ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}
            , ExternsImport {eiModule = ModuleName "B", eiImportType = Implicit, eiImportedAs = Nothing}
            ]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName =
                    Ident "fn"
                , edValueType =
                    ForAll
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        "a2"
                        (Just (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( ConstrainedType
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            (Constraint {constraintAnn = (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []), constraintClass = Qualified (ByModuleName (ModuleName "B")) (ProperName {runProperName = "CsB"}), constraintKindArgs = [], constraintArgs = [TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a2"], constraintData = Nothing})
                            ( TypeApp
                                (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                ( TypeApp
                                    (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                    (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"})))
                                    ( TypeVar
                                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                        "a2"
                                    )
                                )
                                ( TypeConstructor
                                    (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, [])
                                    ( Qualified
                                        (ByModuleName (ModuleName "Prim"))
                                        (ProperName {runProperName = "Int"})
                                    )
                                )
                            )
                        )
                        Nothing
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}
        }

-- module A where\ntype IntSyn = Int\ntype IntSyn2 = IntSyn\n
externsTypeIntSynInsSyn2 =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports = [TypeRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}) (ProperName {runProperName = "IntSyn"}) (Just []), TypeRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}) (ProperName {runProperName = "IntSyn2"}) (Just [])]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDType
                { edTypeName =
                    ProperName
                        { runProperName = "IntSyn"
                        }
                , edTypeKind =
                    TypeConstructor
                        ( SourceSpan
                            { spanName = "tests/purs/make/A.purs"
                            , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}
                            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}
                            }
                        , []
                        )
                        (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
                , edTypeDeclarationKind = TypeSynonym
                }
            , EDTypeSynonym
                { edTypeSynonymName = ProperName {runProperName = "IntSyn"}
                , edTypeSynonymArguments = []
                , edTypeSynonymType =
                    TypeConstructor
                        ( SourceSpan
                            { spanName =
                                "tests/purs/make/A.purs"
                            , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}
                            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}
                            }
                        , []
                        )
                        (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))
                }
            , EDType {edTypeName = ProperName {runProperName = "IntSyn2"}, edTypeKind = TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})), edTypeDeclarationKind = TypeSynonym}
            , EDTypeSynonym
                { edTypeSynonymName = ProperName {runProperName = "IntSyn2"}
                , edTypeSynonymArguments = []
                , edTypeSynonymType =
                    TypeConstructor
                        ( SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}
                        , []
                        )
                        ( Qualified
                            (ByModuleName (ModuleName "A"))
                            (ProperName {runProperName = "IntSyn"})
                        )
                }
            ]
        , efSourceSpan =
            SourceSpan
                { spanName = "tests/purs/make/A.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd =
                    SourcePos
                        { sourcePosLine = 3
                        , sourcePosColumn = 22
                        }
                }
        }
externsTypeIntSynReExport =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "B"
        , efExports =
            [ ReExportRef
                ( SourceSpan
                    { spanName = "tests/purs/make/B.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}
                    }
                )
                (ExportSource {exportSourceImportedFrom = Just (ModuleName "A"), exportSourceDefinedIn = ModuleName "A"})
                (TypeRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}}) (ProperName {runProperName = "IntSyn"}) (Just []))
            , ModuleRef
                ( SourceSpan
                    { spanName = "tests/purs/make/B.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 11}
                    , spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 19}
                    }
                )
                (ModuleName "E")
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "A", eiImportType = Implicit, eiImportedAs = Just (ModuleName "E")}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations = []
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}}
        }

-- module C where\nimport B as B\ntype SynSyn = B.IntSyn\n
externsTypeIntSynSyn =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "C"
        , efExports =
            [ TypeRef
                (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 23}})
                (ProperName {runProperName = "SynSyn"})
                (Just [])
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "B", eiImportType = Implicit, eiImportedAs = Just (ModuleName "B")}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDType
                { edTypeName =
                    ProperName {runProperName = "SynSyn"}
                , edTypeKind =
                    TypeConstructor
                        ( SourceSpan
                            { spanName = "tests/purs/make/C.purs"
                            , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 15}
                            , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 23}
                            }
                        , []
                        )
                        (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
                , edTypeDeclarationKind = TypeSynonym
                }
            , EDTypeSynonym
                { edTypeSynonymName = ProperName {runProperName = "SynSyn"}
                , edTypeSynonymArguments = []
                , edTypeSynonymType =
                    TypeConstructor
                        (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 15}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 23}}, [])
                        ( Qualified
                            (ByModuleName (ModuleName "A"))
                            ( ProperName
                                { runProperName = "IntSyn"
                                }
                            )
                        )
                }
            ]
        , efSourceSpan =
            SourceSpan
                { spanName =
                    "tests/purs/make/C.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 23}
                }
        }

{-
module C where
import B
import A
fn x = mB x"
-}
externsWithTCUsing =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "C"
        , efExports = [ValueRef (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 12}}) (Ident "fn")]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "B", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "A", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "fn"
                , edValueType =
                    ForAll
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        "a2"
                        (Just (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( ConstrainedType
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            ( Constraint
                                { constraintAnn = (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                                , constraintClass =
                                    Qualified (ByModuleName (ModuleName "B")) (ProperName {runProperName = "CsB"})
                                , constraintKindArgs = []
                                , constraintArgs = [TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a2"]
                                , constraintData = Nothing
                                }
                            )
                            (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "a2")) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))))
                        )
                        Nothing
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 4, sourcePosColumn = 12}}
        }

externsSimple0 =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports = [ValueRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 13}}) (Ident "foo")]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType = ForAll (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "t1" (Just (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})))) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) "t1")) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))) Nothing
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 13}}
        }

-- module A (AB) where\ntype SynA = Int\ndata AB = A SynA | B String\n
externsDataExportedWithoutConstructors =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports =
            [ TypeRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 28}}) (ProperName {runProperName = "AB"}) (Just [])
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDType
                { edTypeName = ProperName {runProperName = "AB"}
                , edTypeKind = TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))
                , edTypeDeclarationKind =
                    DataType
                        Data
                        []
                        [
                            ( ProperName {runProperName = "A"}
                            , [TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 13}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 17}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "SynA"}))]
                            )
                        , (ProperName {runProperName = "B"}, [TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 28}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "String"}))])
                        ]
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 28}}
        }

externsABC =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports =
            [ TypeRef
                (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 32}})
                (ProperName {runProperName = "ABC"})
                ( Just
                    [ ProperName {runProperName = "A"}
                    , ProperName {runProperName = "B"}
                    , ProperName {runProperName = "C"}
                    ]
                )
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDType
                { edTypeName =
                    ProperName {runProperName = "ABC"}
                , edTypeKind =
                    TypeConstructor
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        ( Qualified
                            (ByModuleName (ModuleName "Prim"))
                            (ProperName {runProperName = "Type"})
                        )
                , edTypeDeclarationKind =
                    DataType
                        Data
                        []
                        [
                            ( ProperName {runProperName = "A"}
                            ,
                                [ TypeConstructor
                                    ( SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}}
                                    , []
                                    )
                                    ( Qualified
                                        (ByModuleName (ModuleName "Prim"))
                                        ( ProperName
                                            { runProperName = "Int"
                                            }
                                        )
                                    )
                                ]
                            )
                        , (ProperName {runProperName = "B"}, [TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "String"}))])
                        , (ProperName {runProperName = "C"}, [])
                        ]
                }
            , EDDataConstructor
                { edDataCtorName = ProperName {runProperName = "A"}
                , edDataCtorOrigin = Data
                , edDataCtorTypeCtor = ProperName {runProperName = "ABC"}
                , edDataCtorType =
                    TypeApp
                        (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                        ( TypeApp
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"})))
                            ( TypeConstructor
                                (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}}, [])
                                ( Qualified
                                    (ByModuleName (ModuleName "Prim"))
                                    ( ProperName
                                        { runProperName = "Int"
                                        }
                                    )
                                )
                            )
                        )
                        ( TypeConstructor
                            (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, [])
                            (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "ABC"}))
                        )
                , edDataCtorFields = [Ident "value0"]
                }
            , EDDataConstructor {edDataCtorName = ProperName {runProperName = "B"}, edDataCtorOrigin = Data, edDataCtorTypeCtor = ProperName {runProperName = "ABC"}, edDataCtorType = TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "String"})))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "ABC"}))), edDataCtorFields = [Ident "value0"]}
            , EDDataConstructor {edDataCtorName = ProperName {runProperName = "C"}, edDataCtorOrigin = Data, edDataCtorTypeCtor = ProperName {runProperName = "ABC"}, edDataCtorType = TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "ABC"})), edDataCtorFields = []}
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 32}}
        }
externsABCReexport =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "B"
        , efExports =
            [ ReExportRef
                ( SourceSpan
                    { spanName = "tests/purs/make/B.purs"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 23}
                    }
                )
                ( ExportSource
                    { exportSourceImportedFrom = Just (ModuleName "A")
                    , exportSourceDefinedIn = ModuleName "A"
                    }
                )
                ( TypeRef
                    ( SourceSpan
                        { spanName = "tests/purs/make/B.purs"
                        , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                        , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 23}
                        }
                    )
                    (ProperName {runProperName = "ABC"})
                    (Just [ProperName {runProperName = "A"}])
                )
            , ModuleRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 19}}) (ModuleName "E")
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "A", eiImportType = Explicit [TypeRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}}) (ProperName {runProperName = "ABC"}) (Just [ProperName {runProperName = "A"}])], eiImportedAs = Just (ModuleName "E")}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations = []
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 23}}
        }

-- module B (module E) where\nimport A (ABC(..)) as E\n
externsABCReexportsAllCtors =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "B"
        , efExports =
            [ ReExportRef
                (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 24}})
                (ExportSource {exportSourceImportedFrom = Just (ModuleName "A"), exportSourceDefinedIn = ModuleName "A"})
                ( TypeRef
                    (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 24}})
                    (ProperName {runProperName = "ABC"})
                    ( Just
                        [ ProperName {runProperName = "A"}
                        , ProperName {runProperName = "B"}
                        ]
                    )
                )
            , ModuleRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 19}}) (ModuleName "E")
            ]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "A", eiImportType = Explicit [TypeRef (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}) (ProperName {runProperName = "ABC"}) Nothing], eiImportedAs = Just (ModuleName "E")}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations = []
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 24}}
        }
externsABCImport =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "C"
        , efExports = [ValueRef (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 10}}) (Ident "baz")]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}, ExternsImport {eiModule = ModuleName "B", eiImportType = Implicit, eiImportedAs = Just (ModuleName "B")}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations = [EDValue {edValueName = Ident "baz", edValueType = TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "ABC"})))}]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 10}}
        }

-- module A where\nforeign import ForFoo :: Int\n
withForeignFoo =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports = [ValueRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}) (Ident "foo")]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDValue
                { edValueName = Ident "foo"
                , edValueType =
                    TypeConstructor
                        (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 23}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}, [])
                        ( Qualified
                            (ByModuleName (ModuleName "Prim"))
                            (ProperName {runProperName = "Int"})
                        )
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 26}}
        }

-- roles
-- module A where\nforeign import data Effect :: Type -> Type\ntype role Effect representational\n
externsRoles =
    ExternsFile
        { efVersion =
            "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports = [TypeRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}}) (ProperName {runProperName = "Effect"}) (Just [])]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities = []
        , efTypeFixities = []
        , efDeclarations =
            [ EDType
                { edTypeName = ProperName {runProperName = "Effect"}
                , edTypeKind =
                    TypeApp
                        (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 31}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 43}}, [])
                        (TypeApp (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 31}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 43}}, []) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 31}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 35}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"}))))
                        ( TypeConstructor
                            (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 43}}, [])
                            ( Qualified
                                (ByModuleName (ModuleName "Prim"))
                                (ProperName {runProperName = "Type"})
                            )
                        )
                , edTypeDeclarationKind = ExternData [Representational]
                }
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}}
        }

externsFixity =
    ExternsFile
        { efVersion = "0.15.8"
        , efModuleName = ModuleName "A"
        , efExports = [TypeRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 18}}) (ProperName {runProperName = "ADT"}) (Just [ProperName {runProperName = "A"}, ProperName {runProperName = "B"}]), ValueOpRef (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 18}}) (OpName {runOpName = ":+:"})]
        , efImports = [ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Just (ModuleName "Prim")}, ExternsImport {eiModule = ModuleName "Prim", eiImportType = Implicit, eiImportedAs = Nothing}]
        , efFixities =
            [ ExternsFixity
                { efAssociativity = Infixl
                , efPrecedence = 5
                , efOperator =
                    OpName {runOpName = ":+:"}
                , efAlias =
                    Qualified
                        (ByModuleName (ModuleName "A"))
                        (Right (ProperName {runProperName = "A"}))
                }
            ]
        , efTypeFixities = []
        , efDeclarations =
            [ EDType {edTypeName = ProperName {runProperName = "ADT"}, edTypeKind = TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Type"})), edTypeDeclarationKind = DataType Data [] [(ProperName {runProperName = "A"}, [TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"}))]), (ProperName {runProperName = "B"}, [TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "String"}))])]}
            , EDDataConstructor {edDataCtorName = ProperName {runProperName = "A"}, edDataCtorOrigin = Data, edDataCtorTypeCtor = ProperName {runProperName = "ADT"}, edDataCtorType = TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Int"})))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "ADT"}))), edDataCtorFields = [Ident "value0"]}
            , EDDataConstructor {edDataCtorName = ProperName {runProperName = "B"}, edDataCtorOrigin = Data, edDataCtorTypeCtor = ProperName {runProperName = "ADT"}, edDataCtorType = TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeApp (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeConstructor (SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 22}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 28}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "String"})))) (TypeConstructor (SourceSpan {spanName = "", spanStart = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}, spanEnd = SourcePos {sourcePosLine = 0, sourcePosColumn = 0}}, []) (Qualified (ByModuleName (ModuleName "A")) (ProperName {runProperName = "ADT"}))), edDataCtorFields = [Ident "value0"]}
            ]
        , efSourceSpan = SourceSpan {spanName = "tests/purs/make/A.purs", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 18}}
        }

-- PARSED CONTENT

-- parseFromFile "fp" "module A where\ntype IntSyn = Int\n"
parsedInSyn =
    Module
        (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}})
        []
        (ModuleName "A")
        [ TypeSynonymDeclaration
            ( SourceSpan
                { spanName = "fp"
                , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}
                }
            , []
            )
            (ProperName {runProperName = "IntSyn"})
            []
            (TypeConstructor (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 15}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 18}}, []) (Qualified (BySourcePos (SourcePos {sourcePosLine = 0, sourcePosColumn = 0})) (ProperName {runProperName = "Int"})))
        ]
        Nothing

-- mCContent = "module C where\nimport B as B\nbaz = B.foo\n"
resPartial =
    Module
        ( SourceSpan
            { spanName = "tests/purs/make/C.purs"
            , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}
            }
        )
        []
        (ModuleName "C")
        [ ImportDeclaration
            ( SourceSpan
                { spanName =
                    "tests/purs/make/C.purs"
                , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}
                }
            , []
            )
            (ModuleName "B")
            Implicit
            (Just (ModuleName "B"))
        ]
        Nothing

resFull =
    ( []
    , Right
        ( Module
            ( SourceSpan
                { spanName =
                    "tests/purs/make/C.purs"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}
                }
            )
            []
            (ModuleName "C")
            [ ImportDeclaration
                ( SourceSpan
                    { spanName = "tests/purs/make/C.purs"
                    , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}
                    , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}
                    }
                , []
                )
                (ModuleName "B")
                Implicit
                (Just (ModuleName "B"))
            , ValueDeclaration
                ( ValueDeclarationData
                    { valdeclSourceAnn =
                        ( SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}
                        , []
                        )
                    , valdeclIdent = Ident "baz"
                    , valdeclName = Public
                    , valdeclBinders = []
                    , valdeclExpression =
                        [ GuardedExpr [] (PositionedValue (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}) [] (Var (SourceSpan {spanName = "tests/purs/make/C.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}) (Qualified (ByModuleName (ModuleName "B")) (Ident "foo"))))
                        ]
                    }
                )
            ]
            Nothing
        )
    )

-- >>> parseFromFile "fp" "module B (module E) where\nimport A as E"
parsedReexport =
    Module
        ( SourceSpan
            { spanName = "fp"
            , spanStart =
                SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
            , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}
            }
        )
        []
        (ModuleName "B")
        [ ImportDeclaration
            ( SourceSpan
                { spanName = "fp"
                , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 14}
                }
            , []
            )
            (ModuleName "A")
            Implicit
            (Just (ModuleName "E"))
        ]
        ( Just
            [ ModuleRef
                ( SourceSpan
                    { spanName = "fp"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 11}
                    , spanEnd =
                        SourcePos
                            { sourcePosLine = 1
                            , sourcePosColumn = 19
                            }
                    }
                )
                (ModuleName "E")
            ]
        )

-- >>> parseFromFile "fp" "module B (module E) where\nimport A (method) as E"
parsedReexportMethod =
    Module
        (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 23}})
        []
        (ModuleName "B")
        [ ImportDeclaration
            (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 23}}, [])
            (ModuleName "A")
            ( Explicit
                [ ValueRef
                    ( SourceSpan
                        { spanName = "fp"
                        , spanStart =
                            SourcePos {sourcePosLine = 2, sourcePosColumn = 11}
                        , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 17}
                        }
                    )
                    (Ident "method")
                ]
            )
            (Just (ModuleName "E"))
        ]
        ( Just
            [ ModuleRef
                ( SourceSpan
                    { spanName = "fp"
                    , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 11}
                    , spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 19}
                    }
                )
                (ModuleName "E")
            ]
        )

-- parseFromFile "fp" "module B where import A (ABC(..)) as E"
parsedReexportABCAllCtros =
    Module
        ( SourceSpan
            { spanName = "fp"
            , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
            , spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 39}
            }
        )
        []
        (ModuleName "B")
        [ ImportDeclaration
            ( SourceSpan
                { spanName =
                    "fp"
                , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 16}
                , spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 39}
                }
            , []
            )
            (ModuleName "A")
            ( Explicit
                [ TypeRef
                    (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 33}})
                    (ProperName {runProperName = "ABC"})
                    Nothing
                ]
            )
            (Just (ModuleName "E"))
        ]
        Nothing

-- if imports ABC(..) ctors is Nothing
-- if imports ABC ctors is Just([])

-- parseFromFile "fp" "module B where import A (ABC) as E"
parsedReexportABCWithoutCtros =
    Module
        (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 35}})
        []
        (ModuleName "B")
        [ ImportDeclaration
            (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 16}, spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 35}}, [])
            (ModuleName "A")
            ( Explicit
                [ TypeRef
                    (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 1, sourcePosColumn = 29}})
                    (ProperName {runProperName = "ABC"})
                    (Just [])
                ]
            )
            (Just (ModuleName "E"))
        ]
        Nothing

-- >>> parseFromFile "fp" "module B where\nimport A\nclass CsA a <= CsB a where mB :: a -> Int"
classesHier =
    Module
        ( SourceSpan
            { spanName = "fp"
            , spanStart = SourcePos {sourcePosLine = 1, sourcePosColumn = 1}
            , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}
            }
        )
        []
        (ModuleName "B")
        [ ImportDeclaration
            ( SourceSpan
                { spanName = "fp"
                , spanStart = SourcePos {sourcePosLine = 2, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 2, sourcePosColumn = 9}
                }
            , []
            )
            (ModuleName "A")
            Implicit
            Nothing
        , TypeClassDeclaration
            ( SourceSpan
                { spanName = "fp"
                , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}
                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}
                }
            , []
            )
            (ProperName {runProperName = "CsB"})
            [("a", Nothing)]
            [ Constraint
                { constraintAnn = (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}, [])
                , constraintClass =
                    Qualified
                        (BySourcePos (SourcePos {sourcePosLine = 0, sourcePosColumn = 0}))
                        (ProperName {runProperName = "CsA"})
                , constraintKindArgs = []
                , constraintArgs =
                    [ TypeVar
                        (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 11}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 12}}, [])
                        "a"
                    ]
                , constraintData = Nothing
                }
            ]
            []
            [ TypeDeclaration
                ( TypeDeclarationData
                    { tydeclSourceAnn =
                        ( SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 28}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}
                        , []
                        )
                    , tydeclIdent = Ident "mB"
                    , tydeclType =
                        TypeApp
                            ( SourceSpan
                                { spanName = "fp"
                                , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}
                                , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}
                                }
                            , []
                            )
                            (TypeApp (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (TypeConstructor (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 36}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 38}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"}))) (TypeVar (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 35}}, []) "a"))
                            (TypeConstructor (SourceSpan {spanName = "fp", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 39}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 42}}, []) (Qualified (BySourcePos (SourcePos {sourcePosLine = 0, sourcePosColumn = 0})) (ProperName {runProperName = "Int"})))
                    }
                )
            ]
        ]
        Nothing

-- fn :: forall a. A.A a => a -> Int
typeDeclarationData =
    TypeDeclarationData
        { tydeclSourceAnn = (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 1}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}}, [])
        , tydeclIdent = Ident "fn"
        , tydeclType =
            ForAll
                ( SourceSpan
                    { spanName = "tests/purs/make/B.purs"
                    , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 7}
                    , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}
                    }
                , []
                )
                "a"
                Nothing
                ( ConstrainedType
                    ( SourceSpan
                        { spanName = "tests/purs/make/B.purs"
                        , spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 17}
                        , spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}
                        }
                    , []
                    )
                    ( Constraint
                        { constraintAnn = (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 17}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}, [])
                        , constraintClass =
                            Qualified
                                (ByModuleName (ModuleName "A"))
                                (ProperName {runProperName = "A"})
                        , constraintKindArgs = []
                        , constraintArgs = [TypeVar (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 21}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 22}}, []) "a"]
                        , constraintData = Nothing
                        }
                    )
                    ( TypeApp
                        (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}}, [])
                        ( TypeApp
                            (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}}, [])
                            (TypeConstructor (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 28}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 30}}, []) (Qualified (ByModuleName (ModuleName "Prim")) (ProperName {runProperName = "Function"})))
                            ( TypeVar
                                (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 26}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 27}}, [])
                                "a"
                            )
                        )
                        ( TypeConstructor
                            (SourceSpan {spanName = "tests/purs/make/B.purs", spanStart = SourcePos {sourcePosLine = 3, sourcePosColumn = 31}, spanEnd = SourcePos {sourcePosLine = 3, sourcePosColumn = 34}}, [])
                            ( Qualified
                                (BySourcePos (SourcePos {sourcePosLine = 0, sourcePosColumn = 0}))
                                (ProperName {runProperName = "Int"})
                            )
                        )
                    )
                )
                Nothing
        }