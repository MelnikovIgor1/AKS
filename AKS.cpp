#include <iostream>
#include <vector>
#include <math.h>

#include <chrono>
using namespace std::chrono;

class Poly {
public:
    std::vector<long long> array;
    long long module;

    Poly(std::vector<long long> array, long long module) :
        array(array),
        module(module) {

    }

    Poly operator*(const Poly& other) {
        if (module != other.module) {
            throw std::bad_exception();
        }
        long long l = array.size() + other.array.size();

        auto answer = std::vector<long long>(l, 0);

        for (long long i = 0; i < array.size(); ++i) {
            for (long long j = 0; j < other.array.size(); ++j) {
                answer[i + j] = (answer[i + j] + array[i]*other.array[j])%module;
            }
        }

        for (long long i = answer.size() - 1; i >= 0; --i) {
            if (answer[i] == 0) {
                answer.resize(answer.size() - 1);
            } else {
                break;
            }
        }

        return Poly(answer, module);
    }

    void reduce(long long r) {
        for (long long i = array.size() - 1; i >= r; --i) {
            array[i - r] = (array[i - r] + array[i]) % module;
            array[i] = 0;
        }

        for (long long i = array.size() - 1; i >= 0; --i) {
            if (array[i] == 0) {
                array.resize(array.size() - 1);
            } else {
                break;
            }
        }
    }

    void fixedSize(long long r) {
        array.resize(r, 0);
    }

    bool cmp(Poly& other, long long r) {
        reduce(r);
        other.reduce(r);
        fixedSize(r);
        other.fixedSize(r);

        for (long long i = 0; i < r; ++i) {
            if (array[i] != other.array[i]) {
                return false;
            }
        }
        return true;
    }
};

auto two_powers(long long num) {
    std::vector<long long> powers;
    long long i = 1;
    long long deg = 0;
    while (i <= num) {
        if (i & num) {
            powers.push_back(deg);
        }
        i <<= 1;
        ++deg;
    }
    return powers;
}

long long my_pow(long long a, long long b) {
    long long cur = a;
    long long r = 1;
    for (long long i = 0; i < 64; ++i) {
        if (b & (1ll << i)) {
            r = (r*cur);
        }
        cur = (cur*cur);
    }

    return r;
}

long long pow(long long a, long long b, long long m) {
    long long cur = a%m;
    long long r = 1;
    for (long long i = 0; i < 64; ++i) {
        if (b & (1ll << i)) {
            r = (r*cur)%m;
        }
        cur = (cur*cur)%m;
    }

    return r;
}

auto step1(long long n) {
    for (long long i = 2; i < ceil(log(n)) + 1; ++i) {
        long long root = ceil(pow(n, 1 / i));
        if (n == my_pow(root, i) || (root - 1 > 1 and my_pow((root - 1), i) == n)) {
            return true;
        }
    }
    return false;
}

long long gcd(long long a, long long b) {
    while (b > 0) {
        long long c = b;
        b = a%b;
        a = c;
    }

    return a;
}

auto step2(long long n) {
    for (long long r = 2; r < 1e10; ++r) {
        if (gcd(n, r) != 1) {
            continue;
        }

        bool flag = true;

        for (long long i = 1; i < floor(log(n)*log(n)); ++i) {
            long long p = pow(n, i, r);

            if (p == 1) {
                flag = false;
                break;
            }
            if (p == 0) {
                flag = false;
                break;
            }
        }

        if (flag) {
            return r;
        }
    }
    throw std::bad_exception();
}

bool step3(long long n, long long r) {
    for (long long a = 1; a < r + 1; ++a) {
        auto g = gcd(n, a);
        if (1 < g && g < n) {
            return true;
        }
    }
    return false;
}


bool step5_part(long long a, long long n, long long r){
    auto p = two_powers(n);
    long long cur = 0;

    auto answer = Poly({1}, n);
    auto cur_pol = Poly({a, 1}, n);


    for (long long i = 0; i < p.back() + 1; ++i) {
        if (p[cur] == i) {
            answer = answer * cur_pol;
            answer.reduce(r);
            ++cur;
        }

        cur_pol = (cur_pol * cur_pol);
        cur_pol.reduce(r);
    }

    auto t = std::vector<long long>(n + 1, 0);
    t[0] = a;
    t[n] = 1;

    auto pp = Poly(t, n);

    return answer.cmp(pp, r);
}

long long phi(long long n) {
    long long amount = 0;
    for (long long k = 1;k < n; ++k) {
        if (gcd(n, k) == 1) {
            amount += 1;
        }
    }
    return amount;
}


bool step5(long long n, long long r) {
    long long M = floor(sqrt(phi(r)) * log(n));
    for (long long a = 0; a < M; ++a) {
        if (! step5_part(a, n, r)) {
            return false;
        }
    }
    return true;
}

bool aksIsPrime(long long n) {
    if (n <= 1) {
        return false;
    }
    if (step1(n)) {
        return false;
    }

    long long r = step2(n);

    if (step3(n, r)) {
        return false;
    }

    if (n <= r) {
        return true;
    }

    if (step5(n, r)) {
        return true;
    }

    return false;
}

bool isPrime(long long n) {
    if (n == 2) {
        return true;
    }

    for (long long i = 2; i < ceil(sqrt(n)) + 1; ++i) {
        if (n%i == 0) {
            return false;
        }
    }

    return true;
}

int main(int argc, char** argv) {
    long long n = atoi(argv[1]);
    long long param = atoi(argv[2]);

    if (param == 0) {
        auto start = high_resolution_clock::now();

        aksIsPrime(n);

        auto stop = high_resolution_clock::now();
        auto duration = duration_cast<microseconds>(stop - start);

        std::cout << duration.count() << std::endl;
    }
    if (param == 1) {
        auto answer = aksIsPrime(n);

        std::cout << answer << std::endl;
    }

    if (param == 2) {
        auto start = high_resolution_clock::now();
        auto answer = aksIsPrime(n);
        auto stop = high_resolution_clock::now();
        auto duration = duration_cast<microseconds>(stop - start);

        std::cout << duration.count() << " " << answer << std::endl;
    }
}
