-- sunfish.lua, a human transpiler work of https://github.com/thomasahle/sunfish
-- original translation by Soumith Chintala / made require'able by WJLIDDY

-- forces one-index correction, be wary

-- Code License: BSD

local SF = {}

-- The table size is the maximum number of elements in the transposition table.
SF.TABLE_SIZE = 1e6

-- This constant controls how much time we spend on looking for optimal moves.
SF.NODES_SEARCHED = 1e2

-- Mate value must be greater than 8*queen + 2*(rook+knight+bishop)
-- King value is set to twice this value such that if the opponent is
-- 8 queens up, but we got the king, we still exceed MATE_VALUE.
SF.MATE_VALUE = 30000

-- Our board is represented as a 120 character string. The padding allows for
-- fast detection of moves that don't stay within the board.
SF.A1, SF.H1, SF.A8, SF.H8 = 91, 98, 21, 28

SF.initial = 
    '         \n' .. --   0 -  9
    '         \n' .. --  10 - 19
    ' rnbqkbnr\n' .. --  20 - 29
    ' pppppppp\n' .. --  30 - 39
    ' ........\n' .. --  40 - 49
    ' ........\n' .. --  50 - 59
    ' ........\n' .. --  60 - 69
    ' ........\n' .. --  70 - 79
    ' PPPPPPPP\n' .. --  80 - 89
    ' RNBQKBNR\n' .. --  90 - 99
    '         \n' .. -- 100 -109
    '          '     -- 110 -119

__1 = 1 -- 1-index correction
-------------------------------------------------------------------------------
-- Move and evaluation tables
-------------------------------------------------------------------------------
SF.N, SF.E, SF.S, SF.W = -10, 1, 10, -1
SF.directions = {
    P = {SF.N, 2*SF.N, SF.N+SF.W, SF.N+SF.E},
    N = {2*SF.N+SF.E, SF.N+2*SF.E, SF.S+2*SF.E, 2*SF.S+SF.E, 2*SF.S+SF.W, SF.S+2*SF.W, SF.N+2*SF.W, 2*SF.N+SF.W},
    B = {SF.N+SF.E, SF.S+SF.E, SF.S+SF.W, SF.N+SF.W},
    R = {SF.N, SF.E, SF.S, SF.W},
    Q = {SF.N, SF.E, SF.S, SF.W, SF.N+SF.E, SF.S+SF.E, SF.S+SF.W, SF.N+SF.W},
    K = {SF.N, SF.E, SF.S, SF.W, SF.N+SF.E, SF.S+SF.E, SF.S+SF.W, SF.N+SF.W}
}

SF.pst = {
    P = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 198, 198, 198, 198, 198, 198, 198, 198, 0,
        0, 178, 198, 198, 198, 198, 198, 198, 178, 0,
        0, 178, 198, 198, 198, 198, 198, 198, 178, 0,
        0, 178, 198, 208, 218, 218, 208, 198, 178, 0,
        0, 178, 198, 218, 238, 238, 218, 198, 178, 0,
        0, 178, 198, 208, 218, 218, 208, 198, 178, 0,
        0, 178, 198, 198, 198, 198, 198, 198, 178, 0,
        0, 198, 198, 198, 198, 198, 198, 198, 198, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    B = {
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 797, 824, 817, 808, 808, 817, 824, 797, 0,
        0, 814, 841, 834, 825, 825, 834, 841, 814, 0,
        0, 818, 845, 838, 829, 829, 838, 845, 818, 0,
        0, 824, 851, 844, 835, 835, 844, 851, 824, 0,
        0, 827, 854, 847, 838, 838, 847, 854, 827, 0,
        0, 826, 853, 846, 837, 837, 846, 853, 826, 0,
        0, 817, 844, 837, 828, 828, 837, 844, 817, 0,
        0, 792, 819, 812, 803, 803, 812, 819, 792, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    N = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 627, 762, 786, 798, 798, 786, 762, 627, 0,
        0, 763, 798, 822, 834, 834, 822, 798, 763, 0,
        0, 817, 852, 876, 888, 888, 876, 852, 817, 0,
        0, 797, 832, 856, 868, 868, 856, 832, 797, 0,
        0, 799, 834, 858, 870, 870, 858, 834, 799, 0,
        0, 758, 793, 817, 829, 829, 817, 793, 758, 0,
        0, 739, 774, 798, 810, 810, 798, 774, 739, 0,
        0, 683, 718, 742, 754, 754, 742, 718, 683, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    R = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 1258, 1263, 1268, 1272, 1272, 1268, 1263, 1258, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    Q = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 2529, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
    K = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 60098, 60132, 60073, 60025, 60025, 60073, 60132, 60098, 0,
        0, 60119, 60153, 60094, 60046, 60046, 60094, 60153, 60119, 0,
        0, 60146, 60180, 60121, 60073, 60073, 60121, 60180, 60146, 0,
        0, 60173, 60207, 60148, 60100, 60100, 60148, 60207, 60173, 0,
        0, 60196, 60230, 60171, 60123, 60123, 60171, 60230, 60196, 0,
        0, 60224, 60258, 60199, 60151, 60151, 60199, 60258, 60224, 0,
        0, 60287, 60321, 60262, 60214, 60214, 60262, 60321, 60287, 0,
        0, 60298, 60332, 60273, 60225, 60225, 60273, 60332, 60298, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0}
}

-------------------------------------------------------------------------------
-- Chess logic
-------------------------------------------------------------------------------
function SF.isspace(s)
   if s == ' ' or s == '\n' then
      return true
   else
      return false
   end
end

SF.special = '. \n'

function SF.isupper(s)
   if s == "." or s == " " or s == "\n" then return false end
   return s:upper() == s
end

function SF.islower(s)
   if s == "." or s == " " or s == "\n" then return false end
   return s:lower() == s
end

-- super inefficient
function SF.swapcase(s)
   local s2 = ''
   for i=1,#s do
      local c = s:sub(i, i)
      if SF.islower(c) then
	 s2 = s2 .. c:upper()
      else
	 s2 = s2 .. c:lower()
      end
   end
   return s2
end

SF.Position = {}

function SF.Position.new(board, score, wc, bc, ep, kp)
   --[[  A state of a chess game
      board -- a 120 char representation of the board
      score -- the board evaluation
      wc -- the castling rights
      bc -- the opponent castling rights
      ep - the en passant square
      kp - the king passant square
   ]]--
   local self = {}
   self.board = board
   self.score = score
   self.wc = wc 
   self.bc = bc
   self.ep = ep
   self.kp = kp
   for k,v in pairs(SF.Position) do self[k] = v end
   return self
end

function SF.Position:genMoves()
   local moves = {}
   -- For each of our pieces, iterate through each possible 'ray' of moves,
   -- as defined in the 'directions' map. The rays are broken e.g. by
   -- captures or immediately in case of pieces such as knights.
   for i = 1 - __1, #self.board - __1 do
      local p = self.board:sub(i + __1, i + __1)
      if SF.isupper(p) and SF.directions[p] then
	 for _, d in ipairs(SF.directions[p]) do
	    local limit = (i+d) + (10000) * d -- fake limit
	    for j=i+d, limit, d do
	       local q = self.board:sub(j + __1, j + __1)
	       -- Stay inside the board
	       if SF.isspace(self.board:sub(j + __1, j + __1)) then break; end
	       -- Castling
	       if i == sf.A1 and q == 'K' and self.wc[0 + __1] then
		  table.insert(moves,  {j, j-2})
	       end
	       if i == sf.H1 and q == 'K' and self.wc[1 + __1] then 
		  table.insert(moves,  {j, j+2})
	       end
	       -- print(p, q, i, d, j)
	       -- No friendly captures
	       if SF.isupper(q) then break; end
	       -- Special pawn stuff
	       if p == 'P' and (d == SF.N+SF.W or d == SF.N+SF.E) and q == '.' and j ~= self.ep and j ~= self.kp then 
		  break; 
	       end
	       if p == 'P' and (d == SF.N or d == 2*SF.N) and q ~= '.' then 
		  break; 
	       end
	       if p == 'P' and d == 2*SF.N and (i < sf.A1+SF.N or self.board:sub(i+SF.N + __1, i+SF.N + __1) ~= '.') then 
		  break; 
	       end
	       -- Move it
	       table.insert(moves, {i, j})
	       -- print(i, j)
	       -- Stop crawlers from sliding
	       if p == 'P' or p == 'N' or p == 'K' then break; end
	       -- No sliding after captures
	       if SF.islower(q) then break; end
	    end
	 end
      end
   end
   return moves
end


function SF.Position:rotate()
   return self.new(
      SF.swapcase(self.board:reverse()), -self.score,
      self.bc, self.wc, 119-self.ep, 119-self.kp)
end

function SF.Position:move(move)
   assert(move) -- move is zero-indexed
   local i, j = move[0 + __1], move[1 + __1]
   local p, q = self.board:sub(i + __1, i + __1), self.board:sub(j + __1, j + __1)
   local function put(board, i, p) 
      return board:sub(1, i-1) .. p .. board:sub(i+1)
   end
   -- Copy variables and reset ep and kp
   local board = self.board
   local wc, bc, ep, kp = self.wc, self.bc, 0, 0
   local score = self.score + self:value(move)
   -- Actual move
   board = put(board, j + __1, board:sub(i + __1, i + __1))
   board = put(board, i + __1, '.')
   -- Castling rights
   if i == SF.A1 then wc = {false, wc[0 + __1]}; end
   if i == SF.H1 then wc = {wc[0 + __1], false}; end
   if j == SF.A8 then bc = {bc[0 + __1], false}; end
   if j == SF.H8 then bc = {false, bc[1 + __1]}; end
   -- Castling
   if p == 'K' then
      wc = {false, false}
      if math.abs(j-i) == 2 then
	 kp = math.floor((i+j)/2)
	 board = put(board, j < i and SF.A1 + __1 or SF.H1 + __1 , '.')
	 board = put(board, kp + __1, 'R')
      end
   end
   -- Special pawn stuff
   if p == 'P' then
      if SF.A8 <= j and j <= SF.H8 then
	 board = put(board, j + __1, 'Q')
      end
      if j - i == 2*SF.N then
	 ep = i + SF.N
      end
      if ((j - i) == SF.N+SF.W or (j - i) == SF.N+SF.E) and q == '.' then
	 board = put(board, j+SF.S + __1, '.')
      end
   end
   -- We rotate the returned position, so it's ready for the next player
   return self.new(board, score, wc, bc, ep, kp):rotate()
end

function SF.Position:value(move)
   local i, j = move[0 + __1], move[1 + __1]
   local p, q = self.board:sub(i + __1, i + __1), self.board:sub(j + __1, j + __1)
   -- Actual move
   local score = SF.pst[p][j + __1] - SF.pst[p][i + __1]
   -- Capture
   if SF.islower(q) then
      score = score + SF.pst[q:upper()][j + __1]
   end
   -- Castling check detection
   if math.abs(j-self.kp) < 2 then
      score = score + SF.pst['K'][j + __1]
   end
   -- Castling
   if p == 'K' and math.abs(i-j) == 2 then
      score = score + SF.pst['R'][math.floor((i+j)/2) + __1]
      score = score - SF.pst['R'][j < i and SF.A1 + __1 or SF.H1 + __1]
   end
   -- Special pawn stuff
   if p == 'P' then
      if SF.A8 <= j and j <= SF.H8 then
	 score = score + SF.pst['Q'][j + __1] - SF.pst['P'][j + __1]
      end
      if j == self.ep then
	 score = score + SF.pst['P'][j+SF.S + __1]
      end
   end
   return score
end

-- the lamest possible and most embarassing namedtuple hasher ordered dict
-- I apologize to the world for writing it.
SF.tp = {}
SF.tp_index = {}
SF.tp_count = 0

function SF.tp_set(pos, val)
   local b1 = pos.bc[1] and 'true' or 'false'
   local b2 = pos.bc[2] and 'true' or 'false'
   local w1 = pos.bc[1] and 'true' or 'false'
   local w2 = pos.bc[2] and 'true' or 'false'
   local hash = pos.board .. ';' .. pos.score .. ';' .. w1 .. ';' .. w2 .. ';' 
      .. b1 .. ';' .. b2 .. ';' .. pos.ep .. ';' .. pos.kp
      SF.tp[hash] = val
   SF.tp_index[#SF.tp_index + 1] = hash
   SF.tp_count = SF.tp_count + 1
end

function SF.tp_get(pos)
   local b1 = pos.bc[1] and 'true' or 'false'
   local b2 = pos.bc[2] and 'true' or 'false'
   local w1 = pos.bc[1] and 'true' or 'false'
   local w2 = pos.bc[2] and 'true' or 'false'
   local hash = pos.board .. ';' .. pos.score .. ';' .. w1 .. ';' .. w2 .. ';' 
      .. b1 .. ';' .. b2 .. ';' .. pos.ep .. ';' .. pos.kp
   return SF.tp[hash]
end

function SF.tp_popitem()
   SF.tp[SF.tp_index[#SF.tp_index]] = nil
   SF.tp_index[#SF.tp_index] = nil
   SF.tp_count = SF.tp_count - 1
end

-------------------------------------------------------------------------------
-- Search logic
-------------------------------------------------------------------------------

SF.nodes = 0

function SF.bound(pos, gamma, depth)
   --[[ returns s(pos) <= r < gamma    if s(pos) < gamma
        returns s(pos) >= r >= gamma   if s(pos) >= gamma ]]--
    nodes = nodes + 1

    -- Look in the table if we have already searched this position before.
    -- We use the table value if it was done with at least as deep a search
    -- as ours, and the gamma value is compatible.
    local entry = SF.tp_get(pos)
    assert(depth)
    if entry ~= nil and entry.depth >= depth and (
            entry.score < entry.gamma and entry.score < gamma or
            entry.score >= entry.gamma and entry.score >= gamma) then
        return entry.score
    end

    -- Stop searching if we have won/lost.
    if math.abs(pos.score) >= SF.MATE_VALUE then
        return pos.score
    end

    -- Null move. Is also used for stalemate checking
    local nullscore = depth > 0 and -SF.bound(pos:rotate(), 1-gamma, depth-3) or pos.score
    --nullscore = -MATE_VALUE*3 if depth > 0 else pos.score
    if nullscore >= gamma then
        return nullscore
    end

    -- We generate all possible, pseudo legal moves and order them to provoke
    -- cuts. At the next level of the tree we are going to minimize the score.
    -- This can be shown equal to maximizing the negative score, with a slightly
    -- adjusted gamma value.
    local best, bmove = -3*SF.MATE_VALUE, nil
    local moves = pos:genMoves()
    local function sorter(a, b) 
       local va = pos:value(a)
       local vb = pos:value(b)
       if va ~= vb then
	  return va > vb
       else
	  if a[1] == b[1] then
	     return a[2] > b[2]
	  else
	     return a[1] < b[1]
	  end
       end
    end
    table.sort(moves, sorter)
    for _,move in ipairs(moves) do
       -- We check captures with the value function, as it also contains ep and kp
       if depth <= 0 and pos:value(move) < 150 then
	  break
       end
       local score = -SF.bound(pos:move(move), 1-gamma, depth-1)
       -- print(move[1] .. ' ' ..  move[2] .. ' ' .. score)
        if score > best then
	   best = score
	   bmove = move
	end
        if score >= gamma then
	   break
	end
    end

    -- If there are no captures, or just not any good ones, stand pat
    if depth <= 0 and best < nullscore then
       return nullscore
    end
    -- Check for stalemate. If best move loses king, but not doing anything
    -- would save us. Not at all a perfect check.
    if depth > 0 and (best <= -SF.MATE_VALUE) and nullscore > -SF.MATE_VALUE then
       best = 0
    end

    -- We save the found move together with the score, so we can retrieve it in
    -- the play loop. We also trim the transposition table in FILO order.
    -- We prefer fail-high moves, as they are the ones we can build our pv from.
    if entry == nil or depth >= entry.depth and best >= gamma then
      SF.tp_set(pos, {depth = depth, score = best, gamma = gamma, move = bmove})
       if SF.tp_count > SF.TABLE_SIZE then
         SF.tp_popitem()
       end
    end
    return best
end

function SF.search(pos, maxn)
   -- Iterative deepening MTD-bi search
   maxn = maxn or SF.NODES_SEARCHED
   nodes = 0 -- the global value "nodes"
   local score

   -- We limit the depth to some constant, so we don't get a stack overflow in
   -- the end game.
   for depth=1,98 do
      -- The inner loop is a binary search on the score of the position.
      -- Inv: lower <= score <= upper
      -- However this may be broken by values from the transposition table,
      -- as they don't have the same concept of p(score). Hence we just use
      -- 'lower < upper - margin' as the loop condition.
      local lower, upper = -3*SF.MATE_VALUE, 3*SF.MATE_VALUE
      while lower < upper - 3 do
	 local gamma = math.floor((lower+upper+1)/2)
	 score = SF.bound(pos, gamma, depth)
	 -- print(nodes, gamma, score)
	 assert(score)
	 if score >= gamma then
	    lower = score
	 end
	 if score < gamma then
	    upper = score
	 end
      end
      assert(score)

      -- print(string.format("Searched %d nodes. Depth %d. Score %d(%d/%d)", nodes, depth, score, lower, upper))

      -- We stop deepening if the global N counter shows we have spent too
      -- long, or if we have already won the game.
      if nodes >= maxn or math.abs(score) >= SF.MATE_VALUE then
	 break
      end
   end

   -- If the game hasn't finished we can retrieve our move from the
   -- transposition table.
   local entry = SF.tp_get(pos)
   if entry ~= nil then
      return entry.move, score
   end
   return nil, score
end


-------------------------------------------------------------------------------
-- User interface
-------------------------------------------------------------------------------

function SF.parse(c)
   if not c then return nil end
   local p, v = c:sub(1,1), c:sub(2,2)
   if not (p and v and tonumber(v)) then return nil end

   local fil, rank = string.byte(p) - string.byte('a'), tonumber(v) - 1
   return sf.A1 + fil - 10*rank
end


function SF.render(i)
   local rank, fil = math.floor((i - sf.A1) / 10), (i - sf.A1) % 10
   return string.char(fil + string.byte('a')) .. tostring(-rank + 1)
end

function SF.ttfind(t, k)
   assert(t)
   if not k then return false end
   for _,v in ipairs(t) do
      if k[1] == v[1] and k[2] == v[2] then
	 return true
      end
   end
   return false
end

SF.strsplit = function(a)
   local out = {}
   while true do
      local pos, _ = a:find('\n')
      if pos then
	 out[#out+1] = a:sub(1, pos-1)
	 a = a:sub(pos+1)
      else
	 out[#out+1] = a
	 break
      end
   end
   return out
end

function SF.printboard(board)
   local l = SF.strsplit(board, '\n')
   for k,v in ipairs(l) do
      for i=1,#v do
	 io.write(v:sub(i,i))
	 io.write('  ')
      end
      io.write('\n')
   end
end


return SF

