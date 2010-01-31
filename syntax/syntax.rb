def validate(s)
    # initial tags
    text = s.gsub(" ", "")
    unless /^<HTML><BODY>(.*)<\/BODY><\/HTML>$/ =~ text
        return false
    end
    body = $~[1]

    # rest of tags
    tags = []
    while tag = body.slice!(/.*?<([^<>]*)>/)
        tags.push($~[1])
    end

    # check balance
    stack = []
    tags.each do |tag|
        case tag
        when 'B', 'I'
            stack.push(tag)
        when /^AHREF=http:\/\/.*\.com$/
            stack.push('A')
        when '/B', '/I', '/A'
            if stack.empty? || '/' + stack.pop != tag
                return false
            end
        else
            return false
        end
    end

    stack.empty?
end

# input
n = gets.to_i

n.times do
    line = gets
    if validate(line)
        puts "Syntax Included"
    else 
        puts "No Syntax Included"
    end
end
