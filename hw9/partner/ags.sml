exception LeftAsExercise of string

functor AgsFun (structure Game : GAME) :> 
    AGS 
     where type Game.Move.move = Game.Move.move
     and   type Game.state     = Game.state
= struct
  structure Game = Game

  fun advice state = 
    let val currplayer = Game.whoseturn state
        fun checkoutcomes(state, m::ms, bestmove, bestresult) = 
          let val nextstate = Game.makemove state m
              val movelist = Game.legalmoves(nextstate)
          in
            case ms 
               of [] => (case Game.outcome(nextstate)
                          of SOME(Player.WINS player) => 
                            if player = currplayer 
                            then {move = SOME(m), expectedOutcome = 
                                                            Player.WINS(player)}
                            else {move = bestmove, expectedOutcome = bestresult}
                            | SOME(Player.TIE) => 
                                  {move = SOME(m), expectedOutcome = Player.TIE}
                            | NONE => checkoutcomes(nextstate, movelist, 
                                                          bestmove, bestresult))

                | x::xs => (case Game.outcome(nextstate)
                             of SOME(Player.WINS player) =>
                                  if player = currplayer 
                                  then {move = SOME(x), expectedOutcome = 
                                                            Player.WINS(player)}
                                  else checkoutcomes(state, xs, bestmove, 
                                                                     bestresult)
                              | SOME(Player.TIE) => 
                                   checkoutcomes(state, xs, SOME(x), Player.TIE)
                              | NONE => checkoutcomes(nextstate, movelist, 
                                                          bestmove, bestresult))
                end
          | checkoutcomes(state, [], bestmove, bestresult) = 
              let exception ContractViolation in raise ContractViolation end
    in
      case Game.outcome(state)
        of SOME(result) => {move = NONE, expectedOutcome = result}
         | NONE => checkoutcomes(state, Game.legalmoves(state), NONE, 
                                    Player.WINS(Player.otherplayer(currplayer)))
    end
        
end