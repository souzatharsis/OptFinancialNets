/**
 * Option.h
 *
 * Copyright(c) 2014
 * Cristiano Arbex Valle
 * All rights reserved.
 */

#ifndef OPTION_H
#define OPTION_H

#include "Util.h"

using std::string;
using std::vector;

class Option {

    protected:
        
        string name;
        string description;
        bool   showOutput;

    public:
        
        // Create and destroy
        Option();
        Option(string name, string description, bool showOutput);
        virtual ~Option();
        
        
        virtual void checkOption(string str, string optionName) {}
        virtual bool wasChanged() {return false;}
        virtual string getValueAsString() {return "";};
    
        // Non virtual methods
        string getName()        { return name; }
        string getDescription() { return description; }
        bool   getShowOutput()  { return showOutput; }
};    


class BoolOption : public Option {

    private:
        
        bool defaultValue;
        bool value;

    public:

        BoolOption(string name, string description, bool showOutput, bool defaultValue);

        ~BoolOption();

        virtual void checkOption(string str, string optionName);

        virtual bool wasChanged();
        
        virtual string getValueAsString();

        bool getValue() { return value; }
};

class IntOption : public Option {

    private:
        
        int defaultValue;
        int value;
        int min;
        int max;

    public:

        IntOption(string name, string description, bool showOutput, int defaultValue, int max, int min);

        ~IntOption();

        virtual void checkOption(string str, string optionName);

        virtual bool wasChanged();
        
        virtual string getValueAsString();

        int getValue() { return value; }
};

class DoubleOption : public Option {

    private:
        
        double defaultValue;
        double value;
        double min;
        double max;

    public:

        DoubleOption(string name, string description, bool showOutput, double defaultValue, double max, double min);

        ~DoubleOption();

        virtual void checkOption(string str, string optionName);

        virtual bool wasChanged();
        
        virtual string getValueAsString();

        double getValue() { return value; }
};

class StringOption : public Option {

    private:
        
        string defaultValue;
        string value;
        vector<string> possibleValues;

    public:

        StringOption(string name, string description, bool showOutput, string defaultValue, vector<string> possibleValues);

        ~StringOption();

        virtual void checkOption(string str, string optionName);

        virtual bool wasChanged();
        
        virtual string getValueAsString();

        string getValue() { return value; }
};

class ArrayOption : public Option {

    private:
        
        vector<int> defaultValue;
        vector<int> value;

    public:

        ArrayOption(string name, string description, bool showOutput);

        ~ArrayOption();

        virtual void checkOption(string str, string optionName);

        virtual bool wasChanged();
        
        virtual string getValueAsString();

        vector<int> getValue() { return value; }
};


class MatrixOption : public Option {

    private:
        
        vector<vector<int>> defaultValue;
        vector<vector<int>> value;

    public:

        MatrixOption(string name, string description, bool showOutput);

        ~MatrixOption();

        virtual void checkOption(string str, string optionName);

        virtual bool wasChanged();
        
        virtual string getValueAsString();

        vector<vector<int>> getValue() { return value; }
};



#endif 


