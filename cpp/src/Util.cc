/**
 * Util.cc
 *
 * Copyright(c) 2010
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#include "Util.h"


#include <string.h>
#include <sstream>
#include <iostream>

#ifdef _WIN32
#include <Windows.h>
#include <stdint.h> // portable: uint64_t   MSVC: __int64 
#include <winsock.h> // portable: uint64_t   MSVC: __int64 

// MSVC defines this in winsock2.h!?
//typedef struct timeval {
//    long tv_sec;
//    long tv_usec;
//} timeval;

//long gettimeofday(struct timeval * tp, struct timezone * tzp)
long gettimeofday(struct timezone * tzp) {
    // Note: some broken versions only have 8 trailing zero's, the correct epoch has 9 trailing zero's
    static const uint64_t EPOCH = ((uint64_t) 116444736000000000ULL);

    SYSTEMTIME  system_time;
    FILETIME    file_time;
    uint64_t    time;

    GetSystemTime( &system_time );
    SystemTimeToFileTime( &system_time, &file_time );
    time =  ((uint64_t)file_time.dwLowDateTime )      ;
    time += ((uint64_t)file_time.dwHighDateTime) << 32;

    //tp->tv_sec  = (long) ((time - EPOCH) / 10000000L);
    //tp->tv_usec = (long) (system_time.wMilliseconds * 1000);
    return (long) (system_time.wMilliseconds * 1000);
}


#else
#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>
#include <sys/unistd.h>
#endif

#include <sys/stat.h>
#include <stdarg.h>
#include <memory>




////////////////////////////
////////////////////////////
////////////////////////////
// OPEN AND CLOSE FILES ////
////////////////////////////

bool Util::openFile(FILE **fp, const char *nome_arquivo, const char *modo)   {

    if(((*fp)=fopen(nome_arquivo, modo))==NULL)   {
        return false;
    }
    return true;
}


bool Util::closeFile(FILE **fp)   {
    if (fclose(*fp))   {
        return false;
    }
    return true;
}

bool Util::fileExists(string name)   {
    struct stat buffer;
    return (stat(name.c_str(), &buffer) == 0);
}


////////////////////////////
////////////////////////////
////////////////////////////
// GET TIME ////////////////
////////////////////////////

float Util::getTime() {
#ifdef _WIN32
    clock_t start = clock();
    return (float)start / CLOCKS_PER_SEC;
#else
    struct tms tempo;
    clock_t tempotic;
    float cpt = sysconf(_SC_CLK_TCK);
    times(&tempo);
    tempotic = tempo.tms_utime ;
    return ( (float) (tempotic/cpt) );
#endif
}


// Get current date/time, format is YYYY-MM-DD.HH:mm:ss
const string Util::getCurrentDateTime() {
    time_t     now = time(0);
    struct tm  tstruct;
    char       buf[80];
    tstruct = *localtime(&now);
    // Visit http://en.cppreference.com/w/cpp/chrono/c/strftime
    // for more information about date/time format
    strftime(buf, sizeof(buf), "%Y-%m-%d.%X", &tstruct);

    return buf;
}


////////////////////////////
////////////////////////////
////////////////////////////
// CONVERSION //////////////
////////////////////////////

int Util::stringToDouble(string text, double& number) {
    std::istringstream ss(text);
    if (ss >> number) return 0;
    else return 1;
}

////////////////////////////
////////////////////////////
////////////////////////////
// GENERAL /////////////////
////////////////////////////

int Util::numDigits(int number) {
    int digits = 0;
    if (number < 0) return 0;
    while (number) {
        number /= 10;
        digits++;
    }
    return digits;
}

////////////////////////////
////////////////////////////
////////////////////////////
// EXCEPTION ///////////////
////////////////////////////


// Added flag -srd=c++11 for this to compile
void Util::throwInvalidArgument(string msg, ...) {
    if (msg.empty()) {
        throw std::invalid_argument("");
    }
    int final_n, n = ((int)msg.size()) * 2; /* reserve 2 times as much as the length of the msg */
    string str;
    std::unique_ptr<char[]> formatted;
    va_list ap;
    while(1) {
        formatted.reset(new char[n]); /* wrap the plain char array into the unique_ptr */
        strcpy(&formatted[0], msg.c_str());
        va_start(ap, msg);
        final_n = vsnprintf(&formatted[0], n, msg.c_str(), ap);
        va_end(ap);
        if (final_n < 0 || final_n >= n)
            n += abs(final_n - n + 1);
        else
            break;
    }
    string thr = formatted.get();
    throw std::invalid_argument(thr.c_str());
}


////////////////////////////
////////////////////////////
////////////////////////////
// PRINT ///////////////////
////////////////////////////

void Util::printIntVector(const vector<int> &vec, int tot, int numPerLine) {
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        count++;
        printf("%*d ", tot, vec[i]);
        if (numPerLine <= 0 || count % numPerLine == 0 || i == vec.size()-1) printf("\n");
    }
    if (vec.size() == 0) printf("\n");
}

void Util::printUnsignedVector(const vector<unsigned> &vec, int tot, int numPerLine) {
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        count++;
        printf("%*d ", tot, vec[i]);
        if (numPerLine <= 0 || count % numPerLine == 0 || i == vec.size()-1) printf("\n");
    }
    if (vec.size() == 0) printf("\n");
}


void Util::printDoubleVector(const vector<double> &vec, int tot, int dec, int numPerLine) {
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        count++;
        printf("%*.*f ", tot, dec, vec[i]);
        if (numPerLine <= 0 || count % numPerLine == 0 || i == vec.size()-1) printf("\n");
    }
    if (vec.size() == 0) printf("\n");
}

void Util::printNonZeroDoubleVector(const vector<double> &vec, int tot, int dec, int numPerLine) {
    
    int digits = numDigits(vec.size());
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        if (vec[i] != 0) {
            count++;
            printf("%0*d %*.*f  ", digits, i, tot, dec, vec[i]);
            if (numPerLine <= 0 || count % numPerLine == 0) printf("\n");
        } 
    }
    if (count == 0 || count % numPerLine != 0) printf("\n");
}


void Util::printTwoDoubleVectors(const vector<double> &vec1, const vector<double> &vec2, int tot1, int dec1, int tot2, int dec2) {
    if (vec1.size() != vec2.size()) {
        printf("Vectors of different size\n");
        return;
    }

    int tam = vec1.size();
    int num = 0;
    while (tam >= 1) {
        num++;
        tam /= 10;
    }
    for (unsigned i = 0; i < vec1.size(); i++) {
        printf("%*d %*.*f %*.*f\n", num, i, tot1, dec1, vec1[i], tot2, dec2, vec2[i]);
    }
}

void Util::printIntMatrix(const vector<vector<int> > &vec, int tot) {
    for (unsigned i = 0; i < vec.size(); i++) {
        printf("%*d ", tot, i);
    }
    printf("\n");
    
    unsigned max = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        if (vec[i].size() > max) max = vec[i].size();
    }
    for (unsigned m = 0; m < max; m++) {
        for (unsigned i = 0; i < vec.size(); i++) {
            if (vec[i].size() <= m) {
                printf("%*s ", tot, "");
            } else {
                printf("%*d ", tot, vec[i][m]);
            }
        }
        printf("\n");
    }
}


void Util::printDoubleMatrix(const vector<vector<double> > &vec, int tot, int dec) {
    for (unsigned i = 0; i < vec.size(); i++) {
        printf("%*d ", tot, i);
    }
    printf("\n");
    
    unsigned max = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        if (vec[i].size() > max) max = vec[i].size();
    }
    for (unsigned m = 0; m < max; m++) {
        for (unsigned i = 0; i < vec.size(); i++) {
            if (vec[i].size() <= m) {
                printf("%*s ", tot, "");
            } else {
                printf("%*.*f ", tot, dec, vec[i][m]);
            }
        }
        printf("\n");
    }
}

void Util::printGraph(const vector<vector<int>> &graph) {
    int digits = numDigits(graph.size());
    for (int i = 0; i < (int) graph.size(); i++) {
        printf("%*d:", digits, i);
        for (int j = 0; j < (int) graph[i].size(); j++) {
            printf(" %*d", digits, graph[i][j]);
        }
        printf("\n");
    }
}

void Util::printGraph(const vector<vector<double>> &graph, int dec) {
    int digits = numDigits(graph.size());
    for (int i = 0; i < (int) graph.size(); i++) {
        printf("%*d:", digits, i);
        for (int j = 0; j < (int) graph[i].size(); j++) {
            printf(" %*.*f", digits+dec+1, dec, graph[i][j]);
        }
        printf("\n");
    }
}


void Util::printFileUnsignedVector(FILE* file, const vector<unsigned> &vec, int numPerLine) {
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        count++;
        fprintf(file, "%d ", vec[i]);
        if (numPerLine <= 0 || count % numPerLine == 0 || i == vec.size()-1) fprintf(file, "\n");
    }
    if (count == 0 || count % numPerLine != 0) fprintf(file, "\n");
}


void Util::printFileIntVector(FILE* file, const vector<int> &vec, int numPerLine) {
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        count++;
        fprintf(file, "%d ", vec[i]);
        if (numPerLine <= 0 || count % numPerLine == 0 || i == vec.size()-1) fprintf(file, "\n");
    }
    if (count == 0 || count % numPerLine != 0) fprintf(file, "\n");
}

void Util::printFileDoubleVector(FILE* file, const vector<double> &vec, int dec, int numPerLine) {
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        count++;
        fprintf(file, "%.*f ", dec, vec[i]);
        if (numPerLine <= 0 || count % numPerLine == 0 || i == vec.size()-1) fprintf(file, "\n");
    }
    if (count == 0 || count % numPerLine != 0) fprintf(file, "\n");
}

void Util::printFileNonZeroDoubleVector(FILE* file, const vector<double> &vec, int dec, int numPerLine) {
    
    int count = 0;
    for (unsigned i = 0; i < vec.size(); i++) {
        if (vec[i] != 0) {
            count++;
            fprintf(file, "%d:%.*f ", i, dec, vec[i]);
            if (numPerLine <= 0 || count % numPerLine == 0) fprintf(file, "\n");
        } 
    }
    if (count == 0 || count % numPerLine != 0) fprintf(file, "\n");
}




////////////////////////////
////////////////////////////
////////////////////////////
// RANDOM NUMBERS //////////
////////////////////////////

double Util::getSeed() {
    struct timeval begin;
    gettimeofday(&begin, 0);
    return begin.tv_usec;
}


void Util::initialiseSeed(int seed) {
    struct timeval begin;
    gettimeofday(&begin, 0);
    if (seed != 0) {
        srand48(seed);
    } else {
        srand48(begin.tv_usec);
    }
}


/**
 * Calcula um numero aleatorio de 0 a resto.
 */
int Util::randomNumber(int resto) {
    if (resto > 0) {
        long r = lrand48();
        return int(r % resto);
    } else {
        return 0;
    }
}


/**
 * Calcula um numero aleatorio de 0 a resto.
 */
unsigned long Util::randomLong(unsigned long resto) {
    if (resto > 0) {
        long r = lrand48();
        return (r % resto);
    } else {
        return 0;
    }
}

/**
 * Calcula um numero aleatorio de 0 a resto.
 */
float Util::randomNumber(float resto) {
    if (resto > 0) {
        double r = drand48();
        return r * resto;
    } else {
        return 0;
    }
}

int Util::randomAlexandre(int resto, double* semente) {

  float xrand;
  int nselect, i, P;
  P = 2147483647;   /* = Mersenne prime (2**31)-1   */
  for (i=1; i<=5; i++) {
    xrand   = Util::randomNumberAlexandre(semente);
    nselect = 1+(*semente/(P/resto));
  }
  printf("Random: %f\n", xrand);
  return nselect;
}

unsigned long Util::randomLongAlexandre(unsigned long resto, double* semente) {
  float xrand;
  int i, P;
  unsigned long nselect;
  P = 2147483647;   /* = Mersenne prime (2**31)-1   */
  for (i=1; i<=5; i++) {
    xrand   = Util::randomNumberAlexandre(semente);
    nselect = 1+(*semente/(P/resto));
  }
  printf("Random: %f\n", xrand);
  return nselect;
}

float Util::randomNumberAlexandre(double* ix) {
        int     b15, b16, xhi, xalo, leftlo, fhi, k, A, P;

        b15     = 32768;        /* = 2**15      */
        b16     = 65536;        /* = 2**16      */
        P       = 2147483647;   /* = Mersenne prime (2**31)-1   */
        A       = 16807;        /* = 7**5       */
        /* pega 15 hi order bits de ix  */
        xhi     = *ix/b16;

        /* pega 16 lo bits de ix e forma o produto lo*/
        xalo    = (*ix-xhi*b16)*A;

        /* pega 15 hi order bits do produto lo  */
        leftlo  = xalo/b16;

        /* dos 31 highest bits do produto todo */
        fhi     = xhi*A+leftlo;

        /* pega ... */
        k       = fhi/b15;

        /* ...os parentesis sao essenciais      */
        *ix     = (((xalo-leftlo*b16)-P)+(fhi-k*b15)*b16)+k;

        /* se necessario adiciona P     */
        if (*ix < 0) *ix = *ix + P;

        /* multiplica por 1/(2**31-1)*/
        return(*ix*4.656612875e-10);
}


/////////////////////////
/////////////////////////
/////////////////////////
/////////////////////////


