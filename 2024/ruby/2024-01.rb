lines = File.readlines('input/2024/01.txt')

def parse_columns(lines)
  left, right = [], []

  lines.map do |line|
    left << line[0..4].to_i
    right << line[8..12].to_i
  end

  [left, right]
end

puts parse_columns(lines).map { |arr| arr.sort }
                         .transpose
                         .inject(0) { |acc, arr| acc + (arr[0] - arr[1]).abs } # 2166959

input = parse_columns(lines)
puts input[0].inject(0) { |acc, left| acc + left * (input[1].filter { |right| left == right }.length) } # 23741109

