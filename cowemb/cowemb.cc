#include <iostream>
#include <vector>

using namespace std;

struct Line {
    Line(int a, int b, int c)
      : a(a),
        b(b),
        c(c)
    {}
    int a, b, c;
};

bool meet(int r, Line line1, Line line2)
{
    int a = line1.a;
    int b = line1.b;
    int e = line1.c;

    int c = line2.a;
    int d = line2.b;
    int f = line2.c;

    long int foo = e*d - b*f;
    long int bar = a*f - c*e;
    long int baz = a*d - b*c;

    return foo*foo + bar*bar <= r*r * baz*baz;
}

int main(int, const char**)
{
    int n, r;
    cin >> n >> r;

    vector<Line> lines;
    for (int i = 0; i < n; ++i) {
        int a, b, c;
        cin >> a >> b >> c;
        lines.push_back(Line(a, b, -c));
    }

    long int count = 0;
    for (int i = 0; i < n; ++i)
        for (int j = i + 1; j < n; ++j) 
            if (meet(r, lines[i], lines[j]))
                ++count;
    
    cout << count << endl;

    return 0;
}

