#include <iostream>
#include <string>
#include <vector>

using namespace std;

struct pen {
    int ax;
    int ay;
    int bx;
    int by;
};

int main(int, const char **)
{
    int n;
    cin >> n;

    pen *pens = new pen[n];

    for (int i = 0; i < n; i++) {
        cin >> pens[i].ax >> pens[i].ay >> pens[i].bx >> pens[i].by;
    }

    int count = 0;
    int max = 0;

    for (int i = 0; i < n; i++) {
        pen p = pens[i];
        int inside = 0;

        for (int j = 0; j < n; j++) {
            if (i == j)
                continue;
            pen q = pens[j];
            if (p.ax < q.ax && p.ay < q.ay && p.bx > q.bx && p.by > q.by)
                inside++;
        }

        if (inside == max) {
            count++;
        }
        else if (inside > max) {
            max = inside;
            count = 1;
        }
    }

    cout << max << " " << count << endl;
}
