#include <iostream>
#include <string>
#include <vector>

using namespace std;

static inline long int distance(long int a, long int b, long int c, long int d)
{
    long int x = c - a;
    long int y = d - b;
    return x*x + y*y;
}

int main(int, const char**)
{
    int n;
    cin >> n;
    vector<long int> xs;
    vector<long int> ys;
    long int x;
    long int y;
    for (int i = 0; i < n; ++i)
    {
        cin >> x >> y;
        xs.push_back(x);
        ys.push_back(y);
    }

    long int d = 0;
    long int q = 0;
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j)
        {
            q = distance(xs[i], ys[i], xs[j], ys[j]);
            d = d < q ? q : d;
        }

    cout << d << endl;

    return 0;
}
