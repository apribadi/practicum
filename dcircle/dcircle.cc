#include <algorithm>
#include <iostream>
#include <string>
#include <deque>

using namespace std;

bool ok(string s, int n)
{
    int length = s.size();
    string str_seg = s.substr(0, n);
    deque<char> segment(str_seg.begin(), str_seg.end());

    int curr, val1, val2;
    curr = val1 = val2 = count(segment.begin(), segment.end(), 'M');

    for (int i = 0; i < length; ++i) {
        if (curr != val1 && curr != val2) {
            if (val1 == val2 && (val1 - curr == 1 || curr - val1 == 1)) {
                val2 = curr;
            } else {
                return false;
            }
        }
        if (segment.front() == 'M')
            --curr;
        segment.pop_front();
        segment.push_back(s[(i + n) % length]);
        if (segment.back() == 'M')
            ++curr;
    }
    
    return true;
}
int main(int, const char**)
{
    int n;
    cin >> n;
    string s;

    for (int i = 0; i < n; ++i) {
        cin >> s;
        bool good = true;
        int length = s.size();
        for (int j = 2; j < length; ++j) {
            if (!ok(s, j)) {
                cout << "0" << endl;
                good = false;
                break;
            }
        }
        if (good)
            cout << "1" << endl;
    }
    return 0;
}


