# input
n = gets.to_i

one = []
two = []
three = []

n.times do
    one.push(gets.to_i)
end
one.reverse!

# alg
steps = 0
if !one.any? { |x| x != 1 }
    puts 0
else
    stuck = one.find { |x| x != 1 }
    if !one.any? { |x| x == ((stuck == 2) ? 3 : 2) }
        stuck = ((stuck == 2) ? 3 : 2)
    end
    case stuck
    when 2
        moving = 3
        dest = three
        dump = two
    when 3
        moving = 2
        dest = two
        dump = three
    end

    while one.any? { |x| x == moving }
        case cow = one.pop
        when 1
            dump.push(cow)
        when moving
            dest.push(cow)
        when stuck
            dump.push(cow)
        end
        steps += 1
    end
    # now, dest should only have the right ones

    # two paths
    a = one.dup
    b = dump.dup
    c = dest.dup
    try1 = 0
    try2 = 0
    # first try
    while dump.any? {|x| x != stuck}
        dest.push(dump.pop)
        try1 += 1
    end
    while one.any? { |x| x != 1 }
        case cow = one.pop
        when 1
            dest.push(cow)
        when stuck
            dump.push(cow)
        end
        try1 += 1
    end
    while dest.any? { |x| x != moving }
        case cow = dest.pop
        when 1
            one.push(cow)
        when stuck
            dump.push(cow)
        end
        try1 += 1
    end

    # second try
    while a.any? {|x| x != 1}
        c.push(a.pop)
        try2 += 1
    end
    while b.any? { |x| x != stuck }
        case cow = b.pop
        when stuck
            c.push(cow)
        when 1
            a.push(cow)
        end
        try2 += 1
    end
    while c.any? { |x| x != moving }
        case cow = c.pop
        when 1
            a.push(cow)
        when stuck
            b.push(cow)
        end
        try2 += 1
    end

    puts(steps + [try1, try2].min)
end
