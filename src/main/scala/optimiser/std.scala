package optimiser

import ast._

object std {
  val pos = Pos("std", 0, 0)
  val standardLibrary = List(
    Program(
      Some(Ident(pos, List("std"))),
      List(),
      List(),
      List(
        Func(
          pos,
          Ident(pos, List("&")),
          Some(List(FuncParam(pos, "a"))),
          true,
          List(
            Match(
              List(
                Call(pos, Ident(pos, List("&"))),
                Call(pos, Ident(pos, List("a"))),
                AddToStackItem(pos, 1)
              ),
              pos,
              List(Call(pos, Ident(pos, List("a"))), NewStackItem(pos, 0))
            )
          )
        ),
        Func(
          pos,
          Ident(pos, List("\\")),
          None,
          true,
          List(
            Match(List(Call(pos, Ident(pos, List("\\")))), pos, List())
          )
        ),
        Func(
          pos,
          Ident(pos, List("+")),
          None,
          true,
          List(
            Match(
              List(Call(pos, Ident(pos, List("+"))), AddToStackItem(pos, 1)),
              pos,
              List()
            )
          )
        ),
        Func(
          pos,
          Ident(pos, List("*")),
          None,
          true,
          List(
            Call(pos, Ident(pos, List("&"))),
            Block(pos, List(NewStackItem(pos, 0), NewStackItem(pos, 0))),
            Loop(
              pos,
              List(
                Match(
                  List(
                    Call(pos, Ident(pos, List("&"))),
                    Block(
                      pos,
                      List(
                        Call(pos, Ident(pos, List("&"))),
                        AddToStackItem(pos, 1),
                        AddToStackItem(pos, 1)
                      )
                    ),
                    Continue(pos, 0)
                  ),
                  pos,
                  List()
                )
              )
            )
          )
        ),
        Func(
          pos,
          Ident(pos, List("-")),
          None,
          true,
          List(
            Match(
              List(
                Call(pos, Ident(pos, List("&"))),
                Block(
                  pos,
                  List(Match(List(), pos, List(NewStackItem(pos, 0))))
                ),
                Call(pos, Ident(pos, List("-")))
              ),
              pos,
              List()
            )
          )
        ),
        Func(
          pos,
          Ident(pos, List("/")),
          None,
          true,
          List(
            Call(pos, Ident(pos, List("&"))),
            Call(pos, Ident(pos, List("&"))),
            NewStackItem(pos, 0),
            Loop(
              pos,
              List(
                Match(
                  List(
                    Call(pos, Ident(pos, List("&"))),
                    Call(pos, Ident(pos, List("&"))),
                    AddToStackItem(pos, 1),
                    Continue(pos, 0)
                  ),
                  pos,
                  List()
                )
              )
            )
          )
        )
      )
    ),
    Program(
      Some(Ident(pos, List("std", "math"))),
      List(Ident(pos, List("std"))),
      List(),
      List(
        Func(
          pos,
          Ident(pos, List("**")),
          None,
          true,
          List(
            Call(pos, Ident(pos, List("&"))),
            Call(pos, Ident(pos, List("&"))),
            NewStackItem(pos, 0),
            Loop(
              pos,
              List(
                Match(
                  List(
                    Call(pos, Ident(pos, List("&"))),
                    Block(
                      pos,
                      List(
                        Call(pos, Ident(pos, List("*"))),
                        Call(pos, Ident(pos, List("&"))),
                        Call(pos, Ident(pos, List("+")))
                      )
                    ),
                    Continue(pos, 0)
                  ),
                  pos,
                  List(Call(pos, Ident(pos, List("\\"))))
                )
              )
            )
          )
        ),
        Func(
          pos,
          Ident(pos, List("//")),
          None,
          true,
          List(
            Call(pos, Ident(pos, List("&"))),
            Call(pos, Ident(pos, List("&"))),
            NewStackItem(pos, 0),
            Loop(
              pos,
              List(
                Call(pos, Ident(pos, List("&"))),
                Call(pos, Ident(pos, List("*"))),
                Call(pos, Ident(pos, List("*"))),
                Call(pos, Ident(pos, List("&"))),
                Block(
                  pos,
                  List(
                    Call(pos, Ident(pos, List("&"))),
                    AddToStackItem(pos, 1),
                    Call(pos, Ident(pos, List("-"))),
                    Block(
                      pos,
                      List(
                        Match(
                          List(
                            AddToStackItem(pos, 1),
                            NewStackItem(pos, 0),
                            AddToStackItem(pos, 1),
                            Call(pos, Ident(pos, List("&"))),
                            Block(
                              pos,
                              List(
                                Match(List(), pos, List(NewStackItem(pos, 0)))
                              )
                            )
                          ),
                          pos,
                          List(NewStackItem(pos, 0))
                        )
                      )
                    )
                  )
                ),
                Call(pos, Ident(pos, List("/"))),
                Block(
                  pos,
                  List(
                    Match(
                      List(
                        Block(pos, List(Match(List(), pos, List()))),
                        Call(pos, Ident(pos, List("&"))),
                        Call(pos, Ident(pos, List("&"))),
                        Block(
                          pos,
                          List(
                            Call(pos, Ident(pos, List("\\"))),
                            AddToStackItem(pos, 1)
                          )
                        ),
                        Continue(pos, 0)
                      ),
                      pos,
                      List(Call(pos, Ident(pos, List("\\"))))
                    )
                  )
                )
              )
            )
          )
        )
      )
    ),
    Program(
      Some(Ident(pos, List("std", "fmt"))),
      List(Ident(pos, List("std")), Ident(pos, List("std", "math"))),
      List(),
      List(
        Func(
          pos,
          Ident(pos, List("|")),
          None,
          true,
          List(
            Call(pos, Ident(pos, List("&"))),
            Call(pos, Ident(pos, List("&"))),
            NewStackItem(pos, 0),
            Loop(
              pos,
              List(
                Call(pos, Ident(pos, List("*"))),
                Call(pos, Ident(pos, List("&"))),
                Block(
                  pos,
                  List(
                    Call(pos, Ident(pos, List("//"))),
                    NewStackItem(pos, 48),
                    Call(pos, Ident(pos, List("+"))),
                    Call(pos, Ident(pos, List("/")))
                  )
                ),
                Call(pos, Ident(pos, List("/"))),
                Block(
                  pos,
                  List(
                    Match(
                      List(
                        AddToStackItem(pos, 1),
                        Call(pos, Ident(pos, List("/"))),
                        Continue(pos, 0)
                      ),
                      pos,
                      List(Call(pos, Ident(pos, List("\\"))))
                    )
                  )
                )
              )
            )
          )
        ),
        Func(
          pos,
          Ident(pos, List("`")),
          None,
          true,
          List(
            Match(
              List(
                AddToStackItem(pos, 1),
                Call(pos, Ident(pos, List("*"))),
                NewStackItem(pos, 10),
                Call(pos, Ident(pos, List("|"))),
                Print(pos),
                Block(pos, List(NewStackItem(pos, 0), NewStackItem(pos, 10))),
                Print(pos),
                Call(pos, Ident(pos, List("&"))),
                Call(pos, Ident(pos, List("`")))
              ),
              pos,
              List(NewStackItem(pos, 0))
            )
          )
        )
      )
    )
  )
  // Func(
  //   pos,
  //   Ident(pos, List("std", "+")),
  //   Some(List()),
  //   false,
  //   List(

  //   )
  // ),

}
