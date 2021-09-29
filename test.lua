sf = require "sunfish"


local pos = sf.Position.new(sf.initial, 0, {true,true}, {true,true}, 0, 0)

--local profiler = require("profiler")
--profiler.start()


while true do
   -- We add some spaces to the board before we print it.
   -- That makes it more readable and pleasing.
   --sf.printboard(pos.board)

   -- We query the user until she enters a legal move.
   local move = nil
   --[[
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
   ]]--



   local result = sf.search(pos)
	 move = result[1]
	 score = result[2]


	print(sf.convmove(move[1])[1])
	print(sf.convmove(move[1])[2])

   -- The black player moves from a rotated position, so we have to
   -- 'back rotate' the move before printing it.
   print("My move:", sf.render(move[0 + __1]) .. sf.render(move[1 + __1]))
   pos = pos:move(move)
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
   sf.printboard(pos:rotate().board)

    assert(move)

end

sf.printboard(pos:rotate().board)
-- Code block and/or called functions to profile --

--profiler.stop()
--profiler.report("profiler.log")
