#ifndef POINT_H
#define POINT_H

#include<bitset>
#include <iostream>
using std::cout;

#include "BinaryPolynomial.h"

typedef BinaryPolynomial U;


const static U A = 1;
const static U B = std::string(
        "111100111000010000010010010000111101101010011100001000111"\
        "111000010011011011111111010001100001111010010011111000100"\
        "111110010100111001101001000000010011010011010101101111111"\
        "111100110001000110110001101001010001010101011101010111011"\
        "011110100101110101100100110110110001100110101110101100010"\
        "100110110101100011000101011100010110111000011000100101100"\
        "101001001001001100010001011111011010101001000110100111000"\
        "110010000001100101011110011"); //3CE10490F6A708FC26DFE8C3D27C4F94E690134D5BFF988D8D28AAEAEDE975936C66BAC536B18AE2DC312CA493117DAA469C640CAF3

// y^2 + xy = x^3 +  A*x^2 + B

class Point
{
    U x;
    U y;
public:
    bool is_on_curve() const{
        return y*y + x*y == x*x*x + A*x*x + B;
    }

    Point(U tx,U ty){ x = tx;y = ty;}
    Point(const Point& p) {x = p.x; y = p.y;}
    Point(){ x = U(0);y = U(0);}

    Point add(Point p){
        U l = (y + p.y) * inv(x+p.x);
        U x3 = l*l + l + x + p.x + A;
        U y3 = l*(x + x3) + x3 + y;
        return Point(x3,y3);
    }

    Point double_point()
    {
        Point res;
        U m = x + y*inv(x);
        res.x = m*m + m + A;
        res.y = x*x + (m + 1)*res.x;
        return res;
    }


    Point operator+(Point p)
    {
        if( !(x == p.x) ){
            return add(p);
        }
        if( y == p.y ){
            return double_point();
        }
        return Point(0,0);
    }

    // multiply by long long val
    Point operator*(long long x)
    {
        Point p = *this;
        Point res = p;
        x = x - 1;

        while (x != 0)
        {
            if ((x % 2) != 0)
            {
                if ((res.x == p.x) || (res.y == p.y))
                    res = res.double_point();
                else
                    res = res + p;
                x = x - 1;
            }
            x = x / 2;
            p = p.double_point();
        }
        return res;
    }

    // multiply by binary int represented as bitset
    template<int number_length>
    Point operator*(std::bitset<number_length> x)
    {
        if(x == 0){
            return Point(0,0);
        }
        Point p = *this;
        Point res = p;

        int pos = 0;
        do{
            x.flip(pos);
        }while(x[pos++] == 1);

        pos = 0;
        for(;pos < number_length;++pos)
        {
            if(x[pos])
            {
                if ((res.x == p.x) || (res.y == p.y))
                    res = res.double_point();
                else
                    res = res + p;
            }
            p = p.double_point();
        }
        return res;
    }

    friend ostream & operator << (ostream &out, const Point &c);
};

template <size_t len>
void get_random_bits(bitset<len> &inp){
    for(int i=0;i <len/8 + 1;++i){
        inp = (inp << 8) ^ std::bitset<len>(rand()%(1<<8));
    }
}

Point generate_point();

#endif // POINT_H
