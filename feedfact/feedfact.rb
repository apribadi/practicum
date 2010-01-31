n, k, m, r = gets.split.map { |x| x.to_i }

csps = []
n.times do
    csps.push gets.split.map { |x| x.to_i }
end

row = { 0 => 0 }
row.default = 0
csps.each do |cost, prod|
    row.dup.each do |total_cost, max_prod|
        new_cost = total_cost + cost
        row[new_cost] = [row[new_cost], max_prod + prod].max
    end
end

es = row.to_a.map do |cost, prod|
         m.to_f - cost.to_f / prod.to_f unless cost > r || prod < k
     end.compact

best = es.max
if best and best > 0
    puts (best * 1000).truncate
else 
    puts -1
end

