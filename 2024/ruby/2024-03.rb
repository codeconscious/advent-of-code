input = File.readlines('input/2024/03.txt').join

p input.scan(/mul\((\d{1,3}),(\d{1,3})\)/)
       .inject(0) { |acc, arr| acc + arr[0].to_i * arr[1].to_i } # 167090022

p input.scan(/(don't\(\)|do\(\))|mul\((\d{1,3}),(\d{1,3})\)/) # 89823704
       .map(&:compact)
       .inject([true, 0]) { |acc, arr|
            if arr.one?
              [arr[0] == "do()", acc[1]]
            else
              [acc[0], acc[0] == false ? acc[1] : acc[1] + (arr[0].to_i * arr[1].to_i)]
            end
          }[1]
