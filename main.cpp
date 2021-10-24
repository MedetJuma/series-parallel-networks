#include <bits/stdc++.h>

using namespace std;

#define khan ios_base::sync_with_stdio(0), cin.tie(0), cout.tie(0);
#define ll long long
#define pii pair < int, int >
#define pb push_back

const int N = 300 + 5, mod = 998244353, INF = 1e9 + 7;

const ll BIG = 1e18;

const int block = 316;

const int tox[] = {0, 0, 1, -1}, toy[] = {1, -1, 0, 0};

const int base = 1000000000;
const int base_digits = 9;
struct bigint {
    vector<int> a;
    int sign;
    int size(){
        if(a.empty())return 0;
        int ans=(a.size()-1)*base_digits;
        int ca=a.back();
        while(ca)
            ans++,ca/=10;
        return ans;
    }
    bigint operator ^(const bigint &v){
        bigint ans=1,a=*this,b=v;
        while(!b.isZero()){
            if(b%2)
                ans*=a;
            a*=a,b/=2;
        }
        return ans;
    }
    string to_string(){
        stringstream ss;
        ss << *this;
        string s;
        ss >> s;
        return s;
    }
    int sumof(){
        string s = to_string();
        int ans = 0;
        for(auto c : s)  ans += c - '0';
        return ans;
    }
    bigint() :
        sign(1) {
    }
 
    bigint(long long v) {
        *this = v;
    }
 
    bigint(const string &s) {
        read(s);
    }
 
    void operator=(const bigint &v) {
        sign = v.sign;
        a = v.a;
    }
 
    void operator=(long long v) {
        sign = 1;
        a.clear();
        if (v < 0)
            sign = -1, v = -v;
        for (; v > 0; v = v / base)
            a.push_back(v % base);
    }
 
    bigint operator+(const bigint &v) const {
        if (sign == v.sign) {
            bigint res = v;
 
            for (int i = 0, carry = 0; i < (int) max(a.size(), v.a.size()) || carry; ++i) {
                if (i == (int) res.a.size())
                    res.a.push_back(0);
                res.a[i] += carry + (i < (int) a.size() ? a[i] : 0);
                carry = res.a[i] >= base;
                if (carry)
                    res.a[i] -= base;
            }
            return res;
        }
        return *this - (-v);
    }
 
    bigint operator-(const bigint &v) const {
        if (sign == v.sign) {
            if (abs() >= v.abs()) {
                bigint res = *this;
                for (int i = 0, carry = 0; i < (int) v.a.size() || carry; ++i) {
                    res.a[i] -= carry + (i < (int) v.a.size() ? v.a[i] : 0);
                    carry = res.a[i] < 0;
                    if (carry)
                        res.a[i] += base;
                }
                res.trim();
                return res;
            }
            return -(v - *this);
        }
        return *this + (-v);
    }
 
    void operator*=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = 0, carry = 0; i < (int) a.size() || carry; ++i) {
            if (i == (int) a.size())
                a.push_back(0);
            long long cur = a[i] * (long long) v + carry;
            carry = (int) (cur / base);
            a[i] = (int) (cur % base);
            //asm("divl %%ecx" : "=a"(carry), "=d"(a[i]) : "A"(cur), "c"(base));
        }
        trim();
    }
 
    bigint operator*(int v) const {
        bigint res = *this;
        res *= v;
        return res;
    }
 
    void operator*=(long long v) {
        if (v < 0)
            sign = -sign, v = -v;
        if(v > base){
            *this = *this * (v / base) * base + *this * (v % base);
            return ;
        }
        for (int i = 0, carry = 0; i < (int) a.size() || carry; ++i) {
            if (i == (int) a.size())
                a.push_back(0);
            long long cur = a[i] * (long long) v + carry;
            carry = (int) (cur / base);
            a[i] = (int) (cur % base);
            //asm("divl %%ecx" : "=a"(carry), "=d"(a[i]) : "A"(cur), "c"(base));
        }
        trim();
    }
 
    bigint operator*(long long v) const {
        bigint res = *this;
        res *= v;
        return res;
    }
 
    friend pair<bigint, bigint> divmod(const bigint &a1, const bigint &b1) {
        int norm = base / (b1.a.back() + 1);
        bigint a = a1.abs() * norm;
        bigint b = b1.abs() * norm;
        bigint q, r;
        q.a.resize(a.a.size());
 
        for (int i = a.a.size() - 1; i >= 0; i--) {
            r *= base;
            r += a.a[i];
            int s1 = r.a.size() <= b.a.size() ? 0 : r.a[b.a.size()];
            int s2 = r.a.size() <= b.a.size() - 1 ? 0 : r.a[b.a.size() - 1];
            int d = ((long long) base * s1 + s2) / b.a.back();
            r -= b * d;
            while (r < 0)
                r += b, --d;
            q.a[i] = d;
        }
 
        q.sign = a1.sign * b1.sign;
        r.sign = a1.sign;
        q.trim();
        r.trim();
        return make_pair(q, r / norm);
    }
 
    bigint operator/(const bigint &v) const {
        return divmod(*this, v).first;
    }
 
    bigint operator%(const bigint &v) const {
        return divmod(*this, v).second;
    }
 
    void operator/=(int v) {
        if (v < 0)
            sign = -sign, v = -v;
        for (int i = (int) a.size() - 1, rem = 0; i >= 0; --i) {
            long long cur = a[i] + rem * (long long) base;
            a[i] = (int) (cur / v);
            rem = (int) (cur % v);
        }
        trim();
    }
 
    bigint operator/(int v) const {
        bigint res = *this;
        res /= v;
        return res;
    }
 
    int operator%(int v) const {
        if (v < 0)
            v = -v;
        int m = 0;
        for (int i = a.size() - 1; i >= 0; --i)
            m = (a[i] + m * (long long) base) % v;
        return m * sign;
    }
 
    void operator+=(const bigint &v) {
        *this = *this + v;
    }
    void operator-=(const bigint &v) {
        *this = *this - v;
    }
    void operator*=(const bigint &v) {
        *this = *this * v;
    }
    void operator/=(const bigint &v) {
        *this = *this / v;
    }
 
    bool operator<(const bigint &v) const {
        if (sign != v.sign)
            return sign < v.sign;
        if (a.size() != v.a.size())
            return a.size() * sign < v.a.size() * v.sign;
        for (int i = a.size() - 1; i >= 0; i--)
            if (a[i] != v.a[i])
                return a[i] * sign < v.a[i] * sign;
        return false;
    }
 
    bool operator>(const bigint &v) const {
        return v < *this;
    }
    bool operator<=(const bigint &v) const {
        return !(v < *this);
    }
    bool operator>=(const bigint &v) const {
        return !(*this < v);
    }
    bool operator==(const bigint &v) const {
        return !(*this < v) && !(v < *this);
    }
    bool operator!=(const bigint &v) const {
        return *this < v || v < *this;
    }
 
    void trim() {
        while (!a.empty() && !a.back())
            a.pop_back();
        if (a.empty())
            sign = 1;
    }
 
    bool isZero() const {
        return a.empty() || (a.size() == 1 && !a[0]);
    }
 
    bigint operator-() const {
        bigint res = *this;
        res.sign = -sign;
        return res;
    }
 
    bigint abs() const {
        bigint res = *this;
        res.sign *= res.sign;
        return res;
    }
 
    long long longValue() const {
        long long res = 0;
        for (int i = a.size() - 1; i >= 0; i--)
            res = res * base + a[i];
        return res * sign;
    }
 
    friend bigint gcd(const bigint &a, const bigint &b) {
        return b.isZero() ? a : gcd(b, a % b);
    }
    friend bigint lcm(const bigint &a, const bigint &b) {
        return a / gcd(a, b) * b;
    }
 
    void read(const string &s) {
        sign = 1;
        a.clear();
        int pos = 0;
        while (pos < (int) s.size() && (s[pos] == '-' || s[pos] == '+')) {
            if (s[pos] == '-')
                sign = -sign;
            ++pos;
        }
        for (int i = s.size() - 1; i >= pos; i -= base_digits) {
            int x = 0;
            for (int j = max(pos, i - base_digits + 1); j <= i; j++)
                x = x * 10 + s[j] - '0';
            a.push_back(x);
        }
        trim();
    }
 
    friend istream& operator>>(istream &stream, bigint &v) {
        string s;
        stream >> s;
        v.read(s);
        return stream;
    }
 
    friend ostream& operator<<(ostream &stream, const bigint &v) {
        if (v.sign == -1)
            stream << '-';
        stream << (v.a.empty() ? 0 : v.a.back());
        for (int i = (int) v.a.size() - 2; i >= 0; --i)
            stream << setw(base_digits) << setfill('0') << v.a[i];
        return stream;
    }
 
    static vector<int> convert_base(const vector<int> &a, int old_digits, int new_digits) {
        vector<long long> p(max(old_digits, new_digits) + 1);
        p[0] = 1;
        for (int i = 1; i < (int) p.size(); i++)
            p[i] = p[i - 1] * 10;
        vector<int> res;
        long long cur = 0;
        int cur_digits = 0;
        for (int i = 0; i < (int) a.size(); i++) {
            cur += a[i] * p[cur_digits];
            cur_digits += old_digits;
            while (cur_digits >= new_digits) {
                res.push_back(int(cur % p[new_digits]));
                cur /= p[new_digits];
                cur_digits -= new_digits;
            }
        }
        res.push_back((int) cur);
        while (!res.empty() && !res.back())
            res.pop_back();
        return res;
    }
 
    typedef vector<long long> vll;
 
    static vll karatsubaMultiply(const vll &a, const vll &b) {
        int n = a.size();
        vll res(n + n);
        if (n <= 32) {
            for (int i = 0; i < n; i++)
                for (int j = 0; j < n; j++)
                    res[i + j] += a[i] * b[j];
            return res;
        }
 
        int k = n >> 1;
        vll a1(a.begin(), a.begin() + k);
        vll a2(a.begin() + k, a.end());
        vll b1(b.begin(), b.begin() + k);
        vll b2(b.begin() + k, b.end());
 
        vll a1b1 = karatsubaMultiply(a1, b1);
        vll a2b2 = karatsubaMultiply(a2, b2);
 
        for (int i = 0; i < k; i++)
            a2[i] += a1[i];
        for (int i = 0; i < k; i++)
            b2[i] += b1[i];
 
        vll r = karatsubaMultiply(a2, b2);
        for (int i = 0; i < (int) a1b1.size(); i++)
            r[i] -= a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            r[i] -= a2b2[i];
 
        for (int i = 0; i < (int) r.size(); i++)
            res[i + k] += r[i];
        for (int i = 0; i < (int) a1b1.size(); i++)
            res[i] += a1b1[i];
        for (int i = 0; i < (int) a2b2.size(); i++)
            res[i + n] += a2b2[i];
        return res;
    }
 
    bigint operator*(const bigint &v) const {
        vector<int> a6 = convert_base(this->a, base_digits, 6);
        vector<int> b6 = convert_base(v.a, base_digits, 6);
        vll a(a6.begin(), a6.end());
        vll b(b6.begin(), b6.end());
        while (a.size() < b.size())
            a.push_back(0);
        while (b.size() < a.size())
            b.push_back(0);
        while (a.size() & (a.size() - 1))
            a.push_back(0), b.push_back(0);
        vll c = karatsubaMultiply(a, b);
        bigint res;
        res.sign = sign * v.sign;
        for (int i = 0, carry = 0; i < (int) c.size(); i++) {
            long long cur = c[i] + carry;
            res.a.push_back((int) (cur % 1000000));
            carry = (int) (cur / 1000000);
        }
        res.a = convert_base(res.a, 6, base_digits);
        res.trim();
        return res;
    }
};

int n, sz[N], ans, csz, cur[N], m;

int cur_num;

vector < pii > a[N];

vector < int > b;

vector < int > g[N], g1[N];

vector < string > ete3s;

vector < pair < int, vector < int > > > vec;

vector < pii > a1[N];

vector < int > cool_a[N];

map < string, vector < pair < bigint, string > > > salmaqtar;

bool used[N], bad;

string ete3[N], ete3_pro[N];

int ete3_sz;

struct salmaq{
    bigint x[N], y[N], coeff;
};

salmaq add(salmaq a, salmaq b){
    salmaq c;
    for(int i = 1; i <= n; i++){
        c.x[i] = a.x[i];
        c.y[i] = a.y[i];
    }
    c.coeff = a.coeff + b.coeff;
    return c;
}

bool checkus(salmaq a, salmaq b){
    for(int i = 1; i <= n; i++){
        if(a.x[i] != b.x[i])
            return 0;
    }
    for(int i = 1; i <= n; i++){
        if(a.y[i] != b.y[i])
            return 0;
    }
    return 1;
}

salmaq weights[2004], weights1[2004];

vector < salmaq > total, total1;

vector < salmaq > sal_tree, sal_tree1;

bigint fact[N];

void weight_dfs(int v, int lvl){
    used[v] = 1;
    weights[v].coeff = 1;
    weights1[v].coeff = 1;
    int p = -1, cnt = 0;
    for(auto e : g[v]){
        if(used[e]){
            bad = 1;
            continue;
        }
        weight_dfs(e, lvl + 1);
        for(int i = 1; i <= n; i++){
            weights[v].x[i] += weights[e].y[i];
            weights[v].y[i] += weights[e].x[i];
            weights1[v].x[i] = weights[v].x[i];
            weights1[v].y[i] = weights[v].y[i];
        }
        weights[v].coeff *= weights[e].coeff;
        ete3[v] += ete3[e];
        ete3[v] += ", ";
        
        sz[v] += sz[e];
        //factorial weights
        weights1[v].coeff *= weights1[e].coeff;
        ete3_pro[v] += ete3_pro[e];
        ete3_pro[v] += ", ";
        if(p != -1 and ete3_pro[e] == ete3_pro[p]){
            cnt++;
        }
        else{
            weights1[v].coeff *= fact[cnt];
            cnt = 1;
        }
        p = e;
    }
    weights1[v].coeff *= fact[cnt];
    if(ete3[v].size() and ete3[v].back() == ' '){
        ete3[v].pop_back();
        ete3[v].pop_back();
        ete3_pro[v].pop_back();
        ete3_pro[v].pop_back();
    }
    if(!g[v].empty()){
        ete3[v] = '(' + ete3[v] + ')';
        ete3_pro[v] = '(' + ete3_pro[v] + ')';
    }
    else{
        ete3[v] += to_string(cur_num);
        ete3_pro[v] += "#";
        cur_num++;
    }
    if(!g[v].empty()){
        weights[v].x[(int)g[v].size() - 1] += 1;
        weights1[v].x[(int)g[v].size() - 1] += 1;
    }
}

int add(int v, vector < int > b){
    int lvl = 1;
    a[lvl].clear();
    a[lvl].pb({1, 1});
    a[lvl + 1].clear();
    int sz_graph = 0;
    int cur1 = a[lvl][0].second, cur = a[lvl][0].first;
    int itr = 0;
    for(int i = 0; i < b.size(); ){
        if(b[i] == -1){
            i++;
            continue;
        }
        for(int j = 1; j <= cur1; j++){
            int x = b[i];
            a[lvl + 1].pb({v, x});
            g[cur].pb(v);
            sz_graph++;
            v++;
            i++;
        }
        itr++;
        if(a[lvl].size() <= itr){
            itr = 0;
            lvl++;
            a[lvl + 1].clear();
        }
        cur = a[lvl][itr].first;
        cur1 = a[lvl][itr].second;
    }
    return sz_graph;
}

void rem(int v){
    while(!g[v].empty()){
        rem(g[v].back());
        g[v].pop_back();
    }
}

void del(){
    rem(g[1].back());
    g[1].pop_back();
}

void clear(){
    cur_num = 1;
    b.clear();
    for(int i = 1; i <= n; i++){
        a[i].clear();
        cool_a[i].clear();
    }
    for(int i = 1; i <= n; i++){
        used[i] = 0;
        sz[i] = 0;
        ete3[i] = "";
        ete3_pro[i] = "";
    }
    for(int i = 1; i <= n; i++){
        for(int j = 0; j < N; j++){
            weights[i].x[j] = 0;
            weights[i].y[j] = 0;

            weights1[i].x[j] = 0;
            weights1[i].y[j] = 0;
        }
        weights[i].coeff = 0;
        weights1[i].coeff = 0;
    }
    bad = 0;
}

bool larger(int v, int u, int cond = 1){
    if(cond == 1){
        int sz = (int)min(g1[v].size(), g[u].size());
        for(int i = 0; i < sz; i++){
            int v1 = g1[v][i];
            int u1 = g[u][i];
            if(larger(v1, u1))
                return 1;
            if(larger(u1, v1, 0))
                return 0;
        }
        if(g1[v].size() > g[u].size())
            return 1;
        return 0;
    }
    else{
        int sz = (int)min(g[v].size(), g1[u].size());
        for(int i = 0; i < sz; i++){
            int v1 = g[v][i];
            int u1 = g1[u][i];
            if(larger(v1, u1, 0))
                return 1;
            if(larger(u1, v1, 1))
                return 0;
        }
        if(g[v].size() > g1[u].size())
            return 1;
        return 0;
    }
}

void dfs(int v, int lvl){
    used[v] = 1;
    sz[v] = 1;
    int cnt = 0;
    for(auto e : g[v]){
        if(used[e]){
            cout << "bad\n";
            for(int i = 1; i <= n; i++){
                for(auto x : g[i]){
                    cout << x << ' ';
                }
                cout << endl;
            }
            cout << v << " and " << e << endl;
            cout << endl;
            bad = 1;
            continue;
        }
        cnt++;
        dfs(e, lvl + 1);
        ete3[v] += ete3[e];
        ete3[v] += ", ";
        sz[v] += sz[e];
    }
    if(ete3[v].size() and ete3[v].back() == ' '){
        ete3[v].pop_back();
        ete3[v].pop_back();
    }
    if(!g[v].empty()){
        ete3[v] = '(' + ete3[v] + ')';
    }
    else{
        ete3[v] += to_string(cur_num);
        cur_num++;
    }
    cool_a[lvl].pb(cnt);
}

int check(){
    for(int i = 1; i <= n; i++){
        if(g[i].size() == 1)
            return 0;
    }
    dfs(1, 1);
    if(bad)
        return 0;
    int cnt = 0;
    for(int i = 1; i <= n; i++){
        if(!g[i].size() and used[i])
            cnt++;
    }
    if(cnt != m)
        return 0;
    for(int i = 1; i <= n; i++){
        for(auto e : cool_a[i]){
            b.pb(e);
        }
        b.pb(-1);
    }
    //CHECKS WHETHER GRAPH B ALREADY EXISTS
    for(auto el : ete3s){
        if(el == ete3[1]){
            return 0;
        }
    }
    //ADDS B TO THE LIST OF POSSIBLE GRAPHS
    int pos = 0, i1 = 0;
    for(auto x : vec){//vector of vector < int > b s
        //g1
        int ca = x.first;
        vector < int > e = x.second;
        if(ca > cnt)
            break;
        int lvl = 1, v = 2;
        for(int i = 1; i <= n; i++){
            a1[i].clear();
            g1[i].clear();
        }
        lvl = 1;
        a1[lvl].clear();
        a1[lvl].pb({1, e[0]});
        a1[lvl + 1].clear();
        int sz_graph = 0;
        int cur1 = a1[lvl][0].second, cur = a1[lvl][0].first;
        int itr = 0;
        for(int i = 1; i < e.size(); ){
            if(e[i] == -1){
                i++;
                continue;
            }
            for(int j = 1; j <= cur1; j++){
                int x1 = e[i];
                a1[lvl + 1].pb({v, x1});
                g1[cur].pb(v);
                sz_graph++;
                v++;
                i++;
            }
            itr++;
            if(a1[lvl].size() <= itr){
                itr = 0;
                lvl++;
                a1[lvl + 1].clear();
            }
            cur = a1[lvl][itr].first;
            cur1 = a1[lvl][itr].second;
        }
        if(larger(1, 1)){//g1 > g
            break;
        }
        pos = i1;
        i1++;
    }
    pos++;
    vec.insert(vec.begin() + pos, {cnt, b});
    ete3s.insert(ete3s.begin() + pos, ete3[1]);
    ete3_sz = (int)ete3s.size();
    clear();
    weight_dfs(1, 1);
    weights1[1].coeff = fact[cnt] / weights1[1].coeff;
    sal_tree.insert(sal_tree.begin() + pos, weights[1]);
    sal_tree1.insert(sal_tree1.begin() + pos, weights1[1]);
    return 1;
}

void print(int type, string s){
    if(type == 1){
        for(int i = 1; i <= n; i++){
            cout << "#" << i << "# ";
            for(auto e : g[i]){
                cout << e << ' ';
            }
            cout << endl;
        }
        cout << "---------" << endl;
    }
    else{
        cout << s << endl;
        for(int i = 1; i <= 20; i++){
            for(auto e : g[i]){
                cout << i << ' ' << e << endl;
            }
        }
        cout << "---------" << endl;
    }
}

void print_salmaq(salmaq x){
    if(x.coeff != 1)
        cout << x.coeff;
    for(int i = 1; i <= n; i++){
        if(x.x[i] != 0){
            if(x.x[i] > 1)
                cout << "x_{" << i << "}^{" << x.x[i] << "}";
            else
                cout << "x_{" << i << "}";
        }
    }
    for(int i = 1; i <= n; i++){
        if(x.y[i] != 0){
            if(x.y[i] > 1)
                cout << "y_{" << i << "}^{" << x.y[i] << "}";
            else
                cout << "y_{" << i << "}";
        }
    }
//    cout << endl;
}

string tos(bigint a){
    string ans = "";
    while(a != 0){
        int r = a % 10;
        ans += r + '0';
        a /= 10;
    }
    reverse(ans.begin(), ans.end());
    return ans;
}

string string_salmaq(salmaq x){
    string ans = "";
    if(x.coeff != 1)
        ans += tos(x.coeff);
    for(int i = 0; i <= n; i++){
        if(x.x[i] != 0){
            if(x.x[i] > 1){
                ans += "x_{" + tos(i);
                ans += "}^{" + tos(x.x[i]);
                ans += "}";
            }
            else
                ans += "x_{" + tos(i) + "}";
        }
    }
    for(int i = 0; i <= n; i++){
        if(x.y[i] != 0){
            if(x.y[i] > 1)
                ans += "y_{" + tos(i) + "}^{" + tos(x.y[i]) + "}";
            else
                ans += "y_{" + tos(i) + "}";
        }
    }
    return ans;
}

void add_salmaq(salmaq a){
    for(auto &e : total){
        if(checkus(e, a)){
            e = add(e, a);
            return;
        }
    }
    total.pb(a);
}

void inv(salmaq &a){
    for(int i = 1; i <= n; i++){
        swap(a.x[i], a.y[i]);
    }
}

void read(){
    for(int i = 1; i < n; i++){
        int u, v;
        cin >> u >> v;
        g[u].pb(v);
    }
}

void rec(int v, int last, int children){
    if(v > n){
        return;
    }
    clear();
    if(children > 1 and check()){
        int cnt = 0;
        for(int i = 1; i <= n; i++){
            if(!g[i].size() and used[i])
                cnt++;
        }
        if(cnt == m){
            bigint coeff = weights1[1].coeff;
            weights1[1].coeff = 1;
            salmaqtar[string_salmaq(weights1[1])].pb({coeff, ete3[1]});
            inv(weights1[1]);
            salmaqtar[string_salmaq(weights1[1])].pb({coeff, ete3[1]});
            ans++;
        }
    }
    for(int i = last; i < ete3_sz; i++){
        int sz = add(v + 1, vec[i].second);
        rec(v + sz, i, children + 1);
        del();
    }
}

int main(){
    khan
    int m1;
    cin >> m1;
    m = m1;
    n = m + m - 1;
    fact[0] = 1;
    for(int i = 1; i <= n; i++){
        fact[i] = fact[i - 1] * i;
    }
    for(int i = 1; i <= n; i++){
        for(int j = 0; j < N; j++){
            weights[i].x[j] = 0;
            weights[i].y[j] = 0;

            weights1[i].x[j] = 0;
            weights1[i].y[j] = 0;
        }
        weights[i].coeff = 0;
        weights1[i].coeff = 0;
    }
    cur[++csz] = 1;
    vec.pb({1, {0, -1}});
    ete3s.pb("(1)");
    weights[1].coeff = 1;
    sal_tree.pb(weights[1]);
    sal_tree1.pb(weights[1]);
    weights[1].coeff = 0;
    ete3_sz = 1;
    for(int i = 2; i <= m1; i++){
        m = i;
        ans = 0;
        n = m + m - 1;
        rec(1, 0, 0);
        cout << endl;
        cout << "FOR " << i << endl;
        int i2 = 0;
        for(auto e : ete3s){
            if(vec[i2].first == m){
                add_salmaq(sal_tree1[i2]);
                inv(sal_tree1[i2]);
                add_salmaq(sal_tree1[i2]);
            }
            i2++;
        }
        for(auto e : total){
            e.coeff = 1;
            cout << string_salmaq(e) << endl;
            for(auto x : salmaqtar[string_salmaq(e)]){
                cout << x.first << ' ' << x.second << endl;
            }
        }
        salmaqtar.clear();
        total.clear();
    }
}
