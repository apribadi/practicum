#include <iostream>
#include <queue>
#include <set>

using namespace std;

long f(int a, int b, int d, long c)
{
    return a*c/d + b;
}

int main(int, const char**)
{
    int c, n;
    int a1, b1, d1;
    int a2, b2, d2;

    cin >> c >> n;
    cin >> a1 >> b1 >> d1;
    cin >> a2 >> b2 >> d2;

    priority_queue<long> queue;
    queue.push(c);

    set<long> seen;
    seen.insert(c);

    long max = c;

    for (int i = 0; i < n; i++)
    {
        long moo = queue.top();
        queue.pop();

        if (moo > max)
            max = moo;

        long moo1 = f(a1, b1, d1, moo);
        if (seen.find(moo1) != seen.end())
        {
            queue.push(moo1);
            seen.insert(moo1);
        }

        long moo2 = f(a2, b2, d2, moo);
        if (seen.find(moo2) != seen.end())
        {
            queue.push(moo2);
            seen.insert(moo2);
        }
    }

    cout << max << endl;

    return 0;
}

