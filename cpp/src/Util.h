/**
 * Util.h
 *
 * Copyright(c) 2016 
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef UTIL_H
#define UTIL_H

// ANSI escape codes to show colours
#define COLOUR_RED     "\x1b[31m"
#define COLOUR_GREEN   "\x1b[32m"
#define COLOUR_YELLOW  "\x1b[33m"
#define COLOUR_BLUE    "\x1b[34m"
#define COLOUR_MAGENTA "\x1b[35m"
#define COLOUR_CYAN    "\x1b[36m"
#define COLOUR_RESET   "\x1b[0m"

#ifdef _WIN32
#define _CRT_SECURE_NO_WARNINGS
#endif

#include <string>
#include <type_traits>

#ifdef _WIN32
namespace detail {

template<typename T>
inline std::string
to_string(T const val, std::false_type /*is_float*/, std::false_type /*is_unsigned*/) {
    return std::to_string(static_cast<long long>(val));
}

template<typename T>
inline std::string
to_string(T const val, std::false_type /*is_float*/, std::true_type /*is_unsigned*/) {
    return std::to_string(static_cast<unsigned long long>(val));
}

template<typename T, typename _>
inline std::string
to_string(T const val, std::true_type /*is_float*/, _) {
    return std::to_string(static_cast<long double>(val));
}

} // namespace detail

template<typename T>
inline std::string to_string(T const val) {
    return detail::to_string(val, std::is_floating_point<T>(), std::is_unsigned<T>());
}
#else 
using std::to_string;
#endif

// Constantes Gerais
#define INFINITO                  2294967296
#define INFINITO_DOUBLE         2294967296.0
#define TOLERANCE                       1e-7

#include <stdlib.h>
#include <stdio.h>
#include <utility>
#include <numeric>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <stdexcept>
#include <limits>
#include <boost/lexical_cast.hpp>

using std::string;
using std::vector;
using std::map;

inline string lex(int l) {
    return boost::lexical_cast<string>(l);
}

////////////////////////////////////////
////////////////////////////////////////
////////////////////////////////////////

class Util {

    private:

    public:

        /**
         * Retorna o tempo de sistema.
         */
        static float getTime();
        static const string getCurrentDateTime();

        /**
         * Open and close files
         */
        static bool openFile(FILE**, const char*, const char*);
        static bool closeFile(FILE**);
        static bool fileExists(string);

        /**
         * Exception handler
         */
        static void throwInvalidArgument(string msg, ...);

        // 0 success, 1 failure
        static int stringToDouble(string text, double& number);

        // General
        static int numDigits(int number);

        // print functions
        static void printIntVector(const vector<int> &vec, int tot = 3, int numPerLine = 0);
        static void printUnsignedVector(const vector<unsigned> &vec, int tot = 3, int numPerLine = 0);
        static void printDoubleVector(const vector<double> &vec, int tot = 5, int dec = 2, int numPerLine = 0);
        static void printTwoDoubleVectors(const vector<double> &vec1, const vector<double> &vec2, int tot1 = 5, int dec1 = 2, int tot2 = 5, int dec2 = 2);
        static void printIntMatrix(const vector<vector<int> > &vec, int tot = 5);
        static void printDoubleMatrix(const vector<vector<double> > &vec, int tot = 5, int dec = 2);
        static void printNonZeroDoubleVector(const vector<double> &vec, int tot = 5, int dec = 2, int numPerLine = 0);

        static void printGraph(const vector<vector<int>> &graph);
        static void printGraph(const vector<vector<double>> &graph, int dec = 2);
 
        static void printFileIntVector(FILE* file, const vector<int> &vec, int numPerLine = 0);
        static void printFileUnsignedVector(FILE* file, const vector<unsigned> &vec, int numPerLine = 0);
        static void printFileDoubleVector(FILE* file, const vector<double> &vec, int dec = 2, int numPerLine = 0);
        static void printFileNonZeroDoubleVector(FILE* file, const vector<double> &vec, int dec = 2, int numPerLine = 0);
      

        /**
         * Ramdon number functions
         */
        static void initialiseSeed(int seed = 0);
        static double getSeed();
        static int randomNumber(int);
        static unsigned long randomLong(unsigned long);
        static float randomNumber(float);
        static int randomAlexandre(int, double*);
        static float randomNumberAlexandre(double*);
        static unsigned long randomLongAlexandre(unsigned long, double*);

        /**
         * Comparators, sorts, permutations
         */
        template <class T1, class T2>
        struct sortPairAsc {
            bool operator()(const std::pair<T1,T2>&left, const std::pair<T1,T2>&right) {
                return left.second < right.second;
            }
        };

        template <class T1, class T2>
        struct sortPairDesc {
            bool operator()(const std::pair<T1,T2>&left, const std::pair<T1,T2>&right) {
                return left.second > right.second;
            }
        };


        template <typename T, typename Compare>
        static std::vector<int> sort_permutation(std::vector<T> const& vec, Compare compare) {
            std::vector<int> p(vec.size());
            std::iota(p.begin(), p.end(), 0);
            std::sort(p.begin(), p.end(),
            [&](int i, int j){ return compare(vec[i], vec[j]); });
            return p;
        }

        template <typename T>
        static std::vector<T> apply_permutation(std::vector<T> const& vec,  std::vector<int> const& p) {
            std::vector<T> sorted_vec(p.size());
            std::transform(p.begin(), p.end(), sorted_vec.begin(), [&](int i){ return vec[i]; });
            return sorted_vec;
        }


};    

#endif 
