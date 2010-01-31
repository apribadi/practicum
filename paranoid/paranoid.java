import java.util.*;

class paranoid
{
    public static void main(String[] args)
    {
        Scanner sc = new Scanner(System.in);
        Integer n = sc.nextInt();

        Map<Integer, Integer> to_cow_head = new HashMap<Integer, Integer>();
        Map<Integer, Integer> to_cow_tail = new HashMap<Integer, Integer>();
        List<Integer> heads = new ArrayList<Integer>();
        List<Integer> tails = new ArrayList<Integer>();

        for (Integer cow = 1; cow <= n; cow++)
        {
            Integer a = sc.nextInt();
            Integer b = sc.nextInt();
            to_cow_head.put(a, cow);
            to_cow_tail.put(b, cow);
            heads.add(a);
            tails.add(b);
        }

        Deque<Integer> stack = new LinkedList<Integer>();
        SortedSet<Integer> bad = new TreeSet<Integer>();

        for (Integer position : positions)
        {
            Integer cow = to_cow.get(position);
            if (bad.contains(cow))
                continue;
            while (stack.peekFirst() != null && 
                   bad.contains(stack.peekFirst()))
            {
                stack.removeFirst();
            }

            if (started.contains(cow))
            {
                Integer top = stack.peekFirst();
                if (cow == top)
                    stack.removeFirst();
                else
                    bad.add(Math.max(cow, top));

            }
            else
            {
                stack.addLast(cow);
                started.add(cow);
            }
            for (int i : stack)
               System.out.println(i);
            System.out.println("end");
        }

        System.out.println(bad.isEmpty() ? n : bad.first() - 1);
    }
}
