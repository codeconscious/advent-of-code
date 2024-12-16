lines = File.readlines('input/2024/02.txt')
parsed_lines = lines.map { |line| line.split(' ').map { |ch| ch.to_i } }

def unsafe_distance?(a, b)
  a == b || (a - b).abs > 3
end

def safe?(line)
  return false if unsafe_distance?(line[0], line[1])

  is_increase = line[0] < line[1]
  line[1..].each_cons(2) do |a, b|
    return false if unsafe_distance?(a, b) || (a < b) != is_increase
  end

  true
end

puts parsed_lines.filter { |line| safe?(line) }.compact.length # 213

puts parsed_lines.filter { |line| line.combination(line.length - 1)
                                      .to_a
                                      .any? { |c| safe?(c) } }
                 .length # 285
