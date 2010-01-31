# input
def input()
    gets.split.map { |x| x.to_i }
end

# n farms, m roads, k minutes
n, m, k = input

# from -> (to, time)
roads = Hash.new { |hash, key| hash[key] = [] }
m.times do
    farm1, farm2, tax = input
    roads[farm1].push [farm2, tax]
    roads[farm2].push [farm1, tax]
end

state = {1 => 0}  # farm => time
k.times do
    new_state = Hash.new 1000000000
    state.each_pair do |here, total|
        roads[here].each do |dest, tax|
            new_state[dest] = [new_state[dest], total + tax].min
        end
    end
    state = new_state
end

puts state[n]
