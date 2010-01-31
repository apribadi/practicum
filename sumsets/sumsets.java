import java.util.*;

class sumsets
{
    public static void main(String[] args)
    {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();

        int max_pow = (int)Math.ceil(Math.log(n) / Math.log(2));

        long[][] dp = new long[max_pow + 1][n + 1];

        for (int pwr = 0; pwr <= max_pow; pwr++)
            dp[0][0] = 1;

        for (int index = 1; index <= n; index++) 
        {
            for (int inc = 1, pwr = 0;
                 inc <= index;
                 inc *= 2, pwr++)
            {
                long acc = 0;
                for (int p = 0; p <= pwr; p++)
                {
                    acc += dp[p][index - inc];
                    acc %= 1000000000;
                }
                dp[pwr][index] = acc;
            }
        }

        long acc = 0;
        for (int pwr = 0; pwr <= max_pow; pwr++)
        {
            acc += dp[pwr][n];
            acc %= 1000000000;
        }

        System.out.println(acc);
    }
}

