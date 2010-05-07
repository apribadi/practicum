#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

int main(int, const char **)
{
    string patstr; cin >> patstr;

    int n = patstr.size();
    const char *pat = patstr.c_str();

    char *check = new char[n];

    for (int len = 1; len <= n; len++) {
        fill(check, check + n, 0);
        for (int off = 0; off + len <= n; off++) {
            if (equal(pat, pat + len, pat + off)) {
                fill(check + off, check + off + len, 1);
            }
        }
        if (find(check, check + n, 0) == check + n) {
            cout << len << endl;
            return 0;
        }
    }
}
