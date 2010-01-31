a = lambda{|n|!(2...n).any?{|d|n%d==0}} # not for 1
b = lambda{|n|n/=11 while n!=0&&n%11!=1;n!=0}
c = lambda{|n|a=0;(a+=n%2;n/=2) while n!=0;a%4==0}
L,H=gets.split.map{|x|x.to_i}
(L..H).each{|x|a=true;b=true
    while b
    if
