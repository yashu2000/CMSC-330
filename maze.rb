#!/usr/local/bin/ruby

# ########################################
# CMSC 330 - Project 1
# ########################################


#-----------------------------------------------------------
#IMPORTANT CLASS DEFINITION DEFINED BY YASHASWI SHARMA

class Cell 
  
  def initialize (x, y)
    @x = x
    @y = y
  end

  def x
    @x      #REMEMBER TO PUT "@"
  end

  def y
    @y      #remember to put "@"
  end

  def x=(new_X)
    @x = new_X
  end

  def y=(new_Y)
    @y = new_Y
  end

  #SORTS BY X VALUE FIRST!!!
  def <=>(other)

    if @x < other.x then
      return -1
    elsif @x > other.x then
      return 1
    else 
      if @y > other.y then 
        return 1
      elsif @y < other.y then 
        return -1
      else
        return 0
      end
    end
  end

  def x_pp_coordinates 

    return 2*@x + 1
  end
  def y_pp_coordinates 

    return 2*@y + 1
  end

  def ==(other) 

    if @x == other.x and @y == other.y then 
      return true 
    else
      return false 
    end
  end

  def eql?(other) 

    return self == other
  end
  def to_s 

    return "(#{@x},#{@y})"
  end

  def hash 

    return (@x.to_s + @y.to_s).to_i
  end
end


#-----------------------------------------------------------
# FUNCTION DECLARATIONS
#-----------------------------------------------------------


#-----------------------------------------------------------
#PART 1

def pt1_mode_open(file)

  line = file.gets
  if line == nil then 
    return nil 
  end

  #since we don't care about the size, we can skip over and go to 
  #the next set of lines to get the required data
  
  o_count = 0
  while line = file.gets do

    if line[0...4] == "path" then
      next
    else
      #acquires the characters by splitting string via white space
      x, y, ds, w = line.split(/\s/,4) 

      if ds != nil && ds.include?("u") && ds.include?("d") && ds.include?("r") && ds.include?("l") then
        o_count+=1
      end

    end
  end

  return o_count
end


def pt1_mode_bridge(file)
  line = file.gets

  if line == nil then 
    return nil 
  end

  x_y_dir_cost = Hash.new 
  while line = file.gets do 
    if line[0...4] == "path" then #not including position 4
     next
    else
      x, y, ds, w = line.split(/\s/,4)

      new_cell = Cell.new(x.to_i, y.to_i)

      x_y_dir_cost[new_cell] = ds
    end
  end

  total_number_of_bridges = 0
  for key in x_y_dir_cost.keys do 

    dir = x_y_dir_cost[key]
    

    if dir.include?("u") then 
      one_after = Cell.new(key.x, key.y - 1)
      two_after = Cell.new(key.x, key.y - 2)

      directionas = x_y_dir_cost[one_after]

      if x_y_dir_cost.key?(one_after) and x_y_dir_cost.key?(two_after) and directionas != nil and directionas.include?("u") then 

        total_number_of_bridges += 1
      end
    end

    if dir.include?("d") then 
      one_after = Cell.new(key.x, key.y + 1)
      two_after = Cell.new(key.x, key.y + 2)

      directionas = x_y_dir_cost[one_after]
      if x_y_dir_cost.key?(one_after) and x_y_dir_cost.key?(two_after) and directionas != nil and directionas.include?("d") then 

        total_number_of_bridges += 1
      end
    end

    if dir.include?("l") then 
      one_after = Cell.new(key.x - 1, key.y)
      two_after = Cell.new(key.x - 2, key.y)

      directionas = x_y_dir_cost[one_after]
      if x_y_dir_cost.key?(one_after) and x_y_dir_cost.key?(two_after) and directionas != nil  and directionas.include?("l") then 

        total_number_of_bridges += 1
      end
    end

    if dir.include?("r") then 

      one_after = Cell.new(key.x + 1, key.y)
      two_after = Cell.new(key.x + 2, key.y)

      directionas = x_y_dir_cost[one_after]
      if x_y_dir_cost.key?(one_after) and x_y_dir_cost.key?(two_after) and directionas != nil and directionas.include?("r") then 

        total_number_of_bridges += 1
      end
    end
  end

  return (total_number_of_bridges/2)
end


#WORKS
def pt1_mode_sort (file)

  line = file.gets

  if line == nil then 
    return nil 
  end

  #Hash keeps track of the 5 different possibilities
  sort_tracker = Hash.new
  
  #add cells to the corresponding array
  for i in (0..4) do
    
    sort_tracker[i] = Array.new
  end

  while line = file.gets do

    if line[0...4] == "path" then
      next
    else

      x, y, ds, w = line.split(/\s/,4)

      #Adds to the appropriate string dependent on how many possibilities exist 
      #forthat specified cell

      curr_cell = Cell.new(x.to_i, y.to_i)

      #FOR THIS COULD I POTENTIALLY ADD CELLS AND THEN SORT THEM VIA SORT METHOD IN ARRAY AND USE A CODE BLOCK??
      #FOR CODE BLOCK I WOULD NEED TO HAVE A SPACESHIP OPERATOR
      if ds =~ /[udlr][udlr][udlr][udlr]/ then
        sort_tracker[4].push(curr_cell)
      elsif ds =~ /[udlr][udlr][udlr]/ then 
        sort_tracker[3].push(curr_cell)
      elsif ds =~ /[udlr][udlr]/ then 
        sort_tracker[2].push(curr_cell)
      elsif ds =~ /[udlr]/ then 
        sort_tracker[1].push(curr_cell)
      else
        sort_tracker[0].push(curr_cell)
      end
    end

  end

  for key in sort_tracker.keys do 
    sort_tracker[key].sort!
  end

  returner_arr_stringos = Array.new 

  for key in sort_tracker.keys do 

    returner_arr_stringos[key] = "#{key}"

    for cell in sort_tracker[key] do 

      cell_str = cell.to_s

      returner_arr_stringos[key] = returner_arr_stringos[key] + ",#{cell_str}"
    end
  end

  #maybe not return as an array of strings
  return returner_arr_stringos
end

#END OF PART 1
#-----------------------------------------------------------
#PART 2
#DONE


def pt2_paths(file)

  line = file.gets

  sz, sx, sy, ex, ey = line.split(/\s/)
  if line ==  nil then 
    return 
  end
  
  cell_directions_cost = Hash.new
  returner_hash_stringos = Hash.new

  while line = file.gets do
    
    #processes paths
    if line[0...4] == "path" then

      p, path_name, x_start, y_start, ds = line.split(/\s/)

      curr_cell = Cell.new(x_start.to_i, y_start.to_i)
      sum = 0
      invaliditos = false
      
      if x_start != nil and x_start != "" and y_start != nil and y_start != "" and x_start.to_i <= sz.to_i and y_start.to_i <= sz.to_i and 
        ds != nil and ds.length > 0 then 
        
        for i in (0...ds.length) do

          if cell_directions_cost[curr_cell].has_key?(ds[i]) then

            sum += cell_directions_cost[curr_cell][ds[i]]

            case ds[i]
            when 'u'
             curr_cell.y=(curr_cell.y - 1)
            when 'd'
              curr_cell.y=(curr_cell.y + 1)
            when 'l'
              curr_cell.x=(curr_cell.x - 1)
            when 'r'
              curr_cell.x=(curr_cell.x + 1)
            end
        
          else
            sum = "invalid path"
            invaliditos = true
            break 
          end
        end

        unless invaliditos then
        
          returner_hash_stringos[path_name] = sum
        end
      else 
        returner_hash_stringos[path_name] = "invalid path"
      end
    
    #saves cell values
    else

      x, y, ds, w = line.split(/\s/,4)

      cell = Cell.new(x.to_i, y.to_i)
      cell_directions_cost[cell] = Hash.new

      w_array = w.split(/\s/)

      i = 0
      for weight in w_array do
        
        cell_directions_cost[cell][ds[i]] = weight.to_f
        i+=1
      end
    end
  end

  #sorts by value 
  vals_arr = returner_hash_stringos.values.sort
  key_arr = returner_hash_stringos.keys

  key_printed_question = Hash.new

  for key in key_arr do 

    key_printed_question[key] = false
  end

  returner_arr_stringos = Array.new
  for val in vals_arr do 

    for key in key_arr do 

      if returner_hash_stringos[key] == val and val != "invalid path" and key_printed_question[key] == false then 
        val1 = "%10.4f" % val
        add_to_returner = "#{val1} #{key}"
        returner_arr_stringos.push(add_to_returner)

        key_printed_question[key] = true
      end
    end
  end

  
  return returner_arr_stringos unless returner_arr_stringos == nil or returner_arr_stringos.empty?

  return "none"
end


#END OF PART 2
#-----------------------------------------------------------
#PART 3

def pt3_print (file) 

  line = file.gets

  if line == nil then return end 

  size, startx, starty, endx, endy = line.split(/\s/)
  start_coordinates = Cell.new(startx.to_i, starty.to_i)  #start coordinates
  end_coordinates = Cell.new(endx.to_i, endy.to_i)        #end coordinates
  size = size.to_i + 0.5                            #size of maze
  
  two_d_arr = Array.new                         #the maze where we save values

  for i in (0...2*size) do 
    two_d_arr[i] = Array.new
  end

  #adds all the pluses and minuses
  for i in (0...2*size) do 

    if (i % 2) == 0 then 

      for j in (0...2*size) do 

        if (j % 2) == 0 then 
          two_d_arr[i][j] = "+"
        else
          two_d_arr[i][j] = "-"
        end
      end
    else 
      for j in (0...2*size) do 

        if (j % 2) == 0 then 
          two_d_arr[i][j] = "|"
        end
      end
    end
  end

  path_saver = Hash.new #saves path data
  cell_and_dir = Hash.new #saves cell data
  while line = file.gets do
    
    if line[0...4] == "path" then
      
      p, name, sx, sy, ds = line.split(/\s/)

      start_cell = Cell.new(sx.to_i, sy.to_i)
      path_saver[name] = Hash.new

      path_saver[name][start_cell] = ds
    else  
      x, y, ds, w = line.split(/\s/,4)

      curr_cell = Cell.new(x.to_i, y.to_i)

      unless cell_and_dir.has_key?(curr_cell) then

        cell_and_dir[curr_cell] = ds
      end
    end
  end


  #creates spaces whereever cells can move to
  for cell in cell_and_dir.keys do 

    x_curr = cell.x_pp_coordinates
    y_curr = cell.y_pp_coordinates

    disha = cell_and_dir[cell]
    #put y_curr
    if disha.include?("u") then 

      two_d_arr[y_curr - 1][x_curr] = " "
    end

    if disha.include?("d") then 
      two_d_arr[y_curr + 1][x_curr] = " "
    end

    if disha.include?("l") then 
      two_d_arr[y_curr][x_curr-1] = " "
    end

    if disha.include?("r") then 
      two_d_arr[y_curr][x_curr+1] = " "
    end
  end

  file.rewind
  path = pt2_paths(file)

  if path.class != String then 
    
    shortest_path = path[0]
    path_name = ""
    for i in (11...shortest_path.length) do 
      path_name = path_name + shortest_path[i]
    end 

    start_cell = path_saver[path_name].keys
    start_cell = start_cell[0]

    ds = path_saver[path_name][start_cell]
    for i in (0..ds.length) do 
      two_d_arr[start_cell.y_pp_coordinates][start_cell.x_pp_coordinates] = "*"

      case ds[i] 
      when "u"
        start_cell.y=(start_cell.y - 1)
      when "d"
        start_cell.y=(start_cell.y + 1)
      when "l"
        start_cell.x=(start_cell.x - 1)
      when "r"
        start_cell.x=(start_cell.x + 1)
      end
    end
  end

  if two_d_arr[start_coordinates.y_pp_coordinates][start_coordinates.x_pp_coordinates] == "*" then 
    two_d_arr[start_coordinates.y_pp_coordinates][start_coordinates.x_pp_coordinates] = "S"
  else
    two_d_arr[start_coordinates.y_pp_coordinates][start_coordinates.x_pp_coordinates] = "s"
  end

  if two_d_arr[end_coordinates.y_pp_coordinates][end_coordinates.x_pp_coordinates] == "*" then 
    two_d_arr[end_coordinates.y_pp_coordinates][end_coordinates.x_pp_coordinates] = "E"
  else
    two_d_arr[end_coordinates.y_pp_coordinates][end_coordinates.x_pp_coordinates] = "e"
  end


  str_returner = ""

  for i in (0...2*size) do 

    for j in (0...2*size) do 

      if two_d_arr[i][j] == nil then 
        two_d_arr[i][j] = " "
      end
      str_returner = str_returner + two_d_arr[i][j]
    end

    str_returner = str_returner + "\n"
  end

  return str_returner.chomp
end

#END OF PART 3
#-----------------------------------------------------------
#PART 4

def pt4_distance (file)
  line = file.gets
  
  if line == nil then 
    return 
  end

  size, startx, starty, ex, ey = line.split(/\s/) #REGEX: /*pattern match*/ 
  start_coordinates = Cell.new(startx.to_i, starty.to_i)
  
  x_y_dir_cost = Hash.new

  while line = file.gets do
    
    if line[0...4] == "path" then
      break
    else  

      x, y, ds, w = line.split(/\s/,4)

      curr_cell = Cell.new(x.to_i, y.to_i)

      unless x_y_dir_cost.has_key?(curr_cell) then

        x_y_dir_cost[curr_cell] = Array.new
      end

      for i in (0...ds.length) do 

        x_y_dir_cost[curr_cell][i] = ds[i]
      end
    end
  end
  #Following is a Breadth First Search Algo provided by TA

  #max_cell = Cell.new(size.to_i-1, size.to_i - 1)

  for x in (0...size.to_i) do 
    for y in (0...size.to_i) do 

      bitch_cell = Cell.new(x,y)

      if x_y_dir_cost.has_key?(bitch_cell) then 
        next
      else
        x_y_dir_cost[bitch_cell] = nil
      end
    end
  end

  toBeVisited = Array.new #queue
  visited = Array.new      #says which cells are visited
  dist_and_cell_saver = Hash.new #saves cell with distance combo

  toBeVisited.push(start_coordinates)
  dist_and_cell_saver[start_coordinates] = 0

  until toBeVisited.empty? do 

    curr = toBeVisited.shift()
    dirs_of_curr = x_y_dir_cost[curr]

    unless dirs_of_curr == nil     
      
      for disha in dirs_of_curr do

        new_cell = nil
        case disha
        when 'u'
          new_cell = Cell.new(curr.x, curr.y - 1)
        when 'd'
          new_cell = Cell.new(curr.x, curr.y + 1)
        when 'l'
          new_cell = Cell.new(curr.x - 1, curr.y)
        when 'r'
          new_cell = Cell.new(curr.x + 1, curr.y)
        end

        broken_early = false

        for cells in toBeVisited do 

          if cells == new_cell then
            broken_early = true
            break
          end
        end
        for cells in visited do 
        
          if cells == new_cell then 
            broken_early = true
            break
          end
        end

        unless broken_early then
          toBeVisited.push(new_cell)
          dist_and_cell_saver[new_cell] = dist_and_cell_saver[curr] + 1
        end

      end
    end
    visited.push(curr)
  end

  #gets values and sorts the values
  val_saver = dist_and_cell_saver.values
  val_saver.sort!

  #gets keys and sorts the keys
  key_saver = dist_and_cell_saver.keys
  key_saver.sort!

  max_distance = val_saver.max 
  returner_arr_stringos = Array.new

  for distance in (0..max_distance) do 
    returner_arr_stringos[distance] = "#{distance}"

    for key in key_saver do 

      if dist_and_cell_saver[key] == distance then 

        key_str = key.to_s
        returner_arr_stringos[distance] = returner_arr_stringos[distance] + ",#{key_str}"
      end
    end
  end

  #return line by line with \n at the end

  str_return = returner_arr_stringos[0]
  for i in (1...returner_arr_stringos.length) do 

    str_return = str_return + "\n" + returner_arr_stringos[i]
  end


  return str_return
end


#END OF PART 4
#-----------------------------------------------------------
#PART 5

def pt5_solve (file) 

  line = file.gets
  if line == nil then 
    return 
  end

  size, startx, starty, ex, ey = line.split(/\s/) #REGEX: /*pattern match*/ 
  
  start_coordinates = Cell.new(startx.to_i, starty.to_i)
  end_coordinates = Cell.new(ex.to_i, ey.to_i)

  cell_and_dir = Hash.new #saves cells and directions after 

  while line = file.gets do
    
    if line[0...4] == "path" then
      break
    else  

      x, y, ds, w = line.split(/\s/,4)

      curr_cell = Cell.new(x.to_i, y.to_i)

      unless cell_and_dir.has_key?(curr_cell) then

        cell_and_dir[curr_cell] = ds
      end
    end
  end

  tbv = Array.new
  visited = Array.new
  dist_and_cell_saver = Hash.new

  tbv.push(start_coordinates)
  dist_and_cell_saver[start_coordinates] = 0

  until tbv.empty? do
    
    curr = tbv.pop 

    possible_dirs = cell_and_dir[curr]
    for i in (0...possible_dirs.length) do
      
      new_cell = nil
      case possible_dirs[i]

      when "u"
        new_cell = Cell.new(curr.x, curr.y - 1)
      when 'd'
        new_cell = Cell.new(curr.x, curr.y + 1)
      when 'l'
        new_cell = Cell.new(curr.x - 1, curr.y)
      when 'r'
        new_cell = Cell.new(curr.x + 1, curr.y)
      end

      present = false
      for cells in visited do
        
        if cells == new_cell then 
          present = true
          break
        end
      end

      unless present then 

        tbv.push(new_cell)
        dist_and_cell_saver[new_cell] = dist_and_cell_saver[curr] + 1
      end

    end
    visited.push(curr)
  end

  for cell in dist_and_cell_saver.keys do 

    if cell == end_coordinates then

      return true
    end
  end

  return false
end



#END OF PART 5
#------------------------------------------------------------
def parse(file)
  puts "Not yet implemented"    
end

#-----------------------------------------------------------
# the following is a parser that reads in a simpler version
# of the maze files.  Use it to get started writing the rest
# of the assignment.  You can feel free to move or modify 
# this function however you like in working on your assignment.

def read_and_print_simple_file(file)
  line = file.gets
  if line == nil then 
    return 
  end

  # read 1st line, must be maze header
  sz, sx, sy, ex, ey = line.split(/\s/) #REGEX: /*pattern match*/ 
  puts "header spec: size=#{sz}, start=(#{sx},#{sy}), end=(#{ex},#{ey})"

  # read additional lines
  while line = file.gets do

    # begins with "path", must be path specification
    if line[0...4] == "path"
      p, name, x, y, ds = line.split(/\s/)
      puts "path spec: #{name} starts at (#{x},#{y}) with dirs #{ds}"

    # otherwise must be cell specification (since maze spec must be valid)
    else
      x, y, ds, w = line.split(/\s/,4)
      puts "cell spec: coordinates (#{x},#{y}) with dirs #{ds}"
      ws = w.split(/\s/)
      ws.each {|w| puts "  weight #{w}"}
    end
  end
end

#------------------------------------------------------------------
#because when we execute main and other methods in public tests
#we aren't executing via command line
def main(command_name, file_name)
  maze_file = open(file_name)

  # perform command
  case command_name
  when "parse"
    parse(maze_file)
  when "print"
    pt3_print(maze_file)
  when "open"
    pt1_mode_open(maze_file)
  when "bridge"
    pt1_mode_bridge(maze_file)
  when "sortcells"
    pt1_mode_sort(maze_file)
  when "paths"
    pt2_paths(maze_file)
  when "distance"
    pt4_distance(maze_file)
  when "solve"
    pt5_solve(maze_file)
  else
    fail "Invalid command"
  end
end



def two_sum(nums, target)
    
  saver
  found = false
  
  for i in (0...nums.size) do 
     
      for j in (0...nums.size) do 
         
          if i != j then 
             
              if nums[i] + nums[j] == target then 
                  saver = [i, j]
                  found = true
                  break
              end
          end
              
          if found then break
      end
  end
              
  if found then return saver
  else return NIL
  end
end