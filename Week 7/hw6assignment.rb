# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  def initialize(point_array, board)
    super
  end
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = All_Pieces | [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [2, 0]]),
                                [[[0, 0], [-1, 0], [-2, 0], [1, 0], [2, 0]], # long (only needs two)
                                 [[0, 0], [0, -1], [0, -2], [0, 1], [0, 2]]],
                                rotations([[0, 0], [0, -1], [1, 0]])]

  # your enhancements here
  def self.next_piece(board, is_cheat=false)
    if is_cheat
      MyPiece.new([[[0, 0]]], board)
    else
      MyPiece.new(All_My_Pieces.sample, board)
    end
  end

end

class MyBoard < Board
  # your enhancements here
  def initialize(game)
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def rotate_180_degree
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def cheat_pieces
    if @score >= 100 && !@is_cheat
      @is_cheat = true
      @score -= 100
    end
  end

  def next_piece
    @current_block = MyPiece.next_piece(self, @is_cheat)
    @is_cheat = false
    @current_pos = nil
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.length() - 1).each{|index|
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] =
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180_degree})
    @root.bind('c', proc {@board.cheat_pieces})
  end

end


