sf = require "sunfish"


local pos = sf.Position.new(sf.initial, 0, {true,true}, {true,true}, 0, 0)

while true do
   -- We add some spaces to the board before we print it.
   -- That makes it more readable and pleasing.
   sf.printboard(pos.board)

   -- We query the user until she enters a legal move.
   local move = nil
   while true do
  print("Your move: ")
  local crdn = io.read()
  move = {sf.parse(crdn:sub(1,2)), sf.parse(crdn:sub(3,4))}
  if move[1] and move[2] and sf.ttfind(pos:genMoves(), move) then
     break
  else
     -- Inform the user when invalid input (e.g. "help") is entered
     print("Invalid input. Please enter a move in the proper format (e.g. g8f6)")
  end
   end
   pos = pos:move(move)

   -- After our move we rotate the board and print it again.
   -- This allows us to see the effect of our move.
   sf.printboard(pos:rotate().board)

   -- Fire up the engine to look for a move.
   local move, score = sf.search(pos)
   -- print(move, score)
   assert(score)
   if score <= -sf.MATE_VALUE then
  print("You won")
  break
   end
   if score >= sf.MATE_VALUE then
  print("You lost")
  break
   end

   assert(move)

   -- The black player moves from a rotated position, so we have to
   -- 'back rotate' the move before printing it.
   print("My move:", sf.render(119-move[0 + __1]) .. sf.render(119-move[1 + __1]))
   pos = pos:move(move)
end