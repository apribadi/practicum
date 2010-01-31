import java.util.*;

class digest
{
    public static void main(String[] args)
    {
        // input
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int s = sc.nextInt();

        List<Integer> lengths = new ArrayList<Integer>();
        List<Integer> tasks = new ArrayList<Integer>();
        for (int i = 0; i < n; i++) 
        {
            int t = sc.nextInt();
            lengths.add(t);
            for (int j = 0; j < t; j++) 
            {
                tasks.add(sc.nextInt());
            }
        }

        // dp algorithm
        Map<Integer, boolean[]> dp = new HashMap<Integer, boolean[]>();
        dp.put(0, new boolean[0]);

        for (Integer task : tasks)
        {
            Map<Integer, boolean[]> new_dp = new HashMap<Integer, boolean[]>();
            for (Map.Entry<Integer, boolean[]> kv : dp.entrySet())
            {
                boolean[] val = kv.getValue();
                Integer key = kv.getKey();

                int len = val.length;
                boolean[] val1 = Arrays.copyOf(val, len + 1);
                val1[len] = false;
                boolean[] val2 = Arrays.copyOf(val, len + 1);
                val2[len] = true;
                
                if (new_dp.containsKey(key))
                {
                    boolean[] old = new_dp.get(key);
                    new_dp.put(key, lexMax(old, val1));
                }
                else
                {
                    new_dp.put(key, val1);
                }

                Integer sum = key + task;
                if (new_dp.containsKey(sum))
                {
                    boolean[] old = new_dp.get(sum);
                    new_dp.put(sum, lexMax(old, val2));
                }
                else
                {
                    new_dp.put(sum, val2);
                }
            }

            dp = new_dp;
        }

        // answer extraction
        boolean[] answer = dp.get(s);
        int i = 0;
        for (int len : lengths)
        {
            for (int j = 0; j < len; j++)
            {
                System.out.print(answer[i] ? "*" : ".");
                i++;
            }
            System.out.println();
        }
    }

    private static boolean[] lexMax(boolean[] rhs, boolean[] lhs)
    {
        int n = Math.min(rhs.length, lhs.length);
        for (int i = 0; i < n; i++)
        {
            if (!rhs[i] && lhs[i])
                return lhs;
            else if (rhs[i] && !lhs[i])
                return rhs;
        }

        return rhs;
    }
}

