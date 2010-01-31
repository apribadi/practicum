# input
n, m = gets.split.map { |x| x.to_i }
capacity = []
mothers = {}
n.times do |i|
    cow = i + 1
    gal, mother = gets.split.map { |x| x.to_i }
    capacity.push([cow, gal])
    mothers[cow] = mother
end

val = lambda { |cows|
    cows.select { |cow| cows.include?(mothers[cow]) }.length
}

# calculations
table = { 0 => [] }
table.default = []
capacity.each do |cow, gal|
    table.dup.each do |total, cows|
        total += gal
        table[total] = [table[total], cows.dup.push(cow)].max { |a, b| val.call(a) <=> val.call(b) }
    end
end

#table.reject! { |total, cows| total < m }
puts table.inspect
puts table.to_a.map { |total, cows| val.call(cows) }.max

