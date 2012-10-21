# encoding: UTF-8

require 'pry'
require 'awesome_print'

trap(:INT) do
  puts 'Life is over'
  exit(0)
end

class Life

  ALIVE = 1
  DEAD = 0

  def initialize(columns)
    @columns = columns - 2
    @prior = nil
    @current = build_generation(true)
    @neighbors = {}
    @generation = 1
  end

  def dead?(v)
    v == DEAD
  end

  def draw
    puts "┌#{"─" * (@columns*2)}┐"
    @current.each do |row|
      print "│" + row.map { |cell| char_for_health(cell) }.join + "│\r"
      puts
    end
    puts "└#{"─" * (@columns*2)}┘"
    puts "generation=#{@generation} population=#{@population}"
  end

  def increment!
    @generation += 1
    @prior = @current
    @current = build_generation
    compute
  end

  def living?(v)
    v == ALIVE
  end

  private

  def build_generation(seed = false)
    if seed
      Array.new(@columns) { Array.new(@columns) { rand.round } }
    else
      Array.new(@columns) { Array.new(@columns, DEAD) }
    end
  end

  def compute
    @population = 0

    @prior.each_with_index do |cols, row|
      cols.each_with_index do |val, col|
        new_value = case
        when living?(val) then
          value_for_living_cell([row, col])
        when dead?(val) then
          value_for_dead_cell([row, col])
        end
        @population += new_value
        @current[row][col] = new_value
      end
    end
  end

  # Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
  def value_for_dead_cell(cell)
    sum = neighboring_sum(cell)
    case
    when sum == 3 then ALIVE
    else DEAD
    end
  end

  # Any live cell with fewer than two live neighbours dies, as if caused by under-population.
  # Any live cell with two or three live neighbours lives on to the next generation.
  # Any live cell with more than three live neighbours dies, as if by overcrowding.
  def value_for_living_cell(cell)
    sum = neighboring_sum(cell)
    case
    when sum < 2, sum > 3 then DEAD
    else ALIVE
    end
  end

  def neighboring_sum(cell)
    map_neighbors_for_cell(cell) unless @neighbors.key?(cell)
    @neighbors[cell].inject(0) { |sum, (row, col)| sum + @prior[row][col] }
  end

  def map_neighbors_for_cell(cell)
    row, col = cell
    rows = [row - 1, row, row + 1]
    cols = [col - 1, col, col + 1]
    rows.delete_if { |v| v < 0 || v >= @columns }
    cols.delete_if { |v| v < 0 || v >= @columns }
    neighbors = rows.product(cols) - [cell]
    @neighbors[cell] = neighbors
  end

  def char_for_health(n)
    living?(n) ? 'x ' : '  '
  end

end

cols = (ARGV.pop || (%x{tput cols}.strip.to_i / 2)).to_i
@board = ::Life.new(cols)

loop do
  system('tput clear')
  @board.draw
  @board.increment!
  sleep 0.2
end
