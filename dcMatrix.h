//
//  dcMatrix.h
//  Epidemic_Models
//
//  Created by David Champredon on 12-05-27.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef dcMatrix_h
#define dcMatrix_h

#include <iostream>
#include <fstream>
#include <math.h>
#include <string.h>
#include <vector>
#include <assert.h>
#include <cstdlib>


using namespace std;

class Matrix
{
public:
	int nbRows;	//Dimensions
	int nbCols;
    
	vector<double> val;	//Value for each element
    
	double & operator()(int,int);
	double & operator[](int);
    
    int getNbRows()     {return nbRows;}
    int getNbCols()     {return nbCols;}
    
    
    // Constructors
	
	//Matrix(){nbRows=0;nbCols=0;}
	Matrix(){}
	
	Matrix(int h,int l) 
	{
		nbRows=h; nbCols=l; 
		vector<double> tmp(l*h,0.0);
		val=tmp;
	}
	
	Matrix(int n) 
	{
		nbRows=n; nbCols=n;
		vector<double> tmp(n*n, 0.0);
		val=tmp;
	}
	
    Matrix(string pathFile);
	
	Matrix(vector<double> v);	// Matrix (n,1) from a single vector(n)
	
	
    
    void    resize(int n)  
	{
		nbRows=n;nbCols=n; 
		val.resize(n*n);
	}
    
	void    resize(int nRows, int nCols)  
	{
		nbRows=nRows; nbCols=nCols; 
		this->val.resize(nRows*nCols);
	}
    
	void    display();
        
	void    RandomInit();
    
    // Files operations
	void    FromFile(const char*);
    void    FromFile(string);
	void	FromFile_Rows(string fileName, int nrow);
	
    void    WriteToFile(string);
	void    WriteToFileCSV(string);
	void    WriteToFileCSV(string fileName, vector<string> header);
	
	
    // Operations on embedded vectors
    vector<double>  extractColumn(int j_col);
	vector<double>	extractRow(int i_row);
	
	void            addRowVector(vector<double> v);
	void            addRowVector(vector<unsigned long> v);
	
    void            addColVector(vector<double> v);
    
	void			removeRow(int i_row);	// removes row 'i_row' and resize Matrix
	void			removeCol(int j_col);	// removes column 'j_col' and resize Matrix

	
	
	// Extract the row #i of the matrix
	// "i" is calculated such that it is
	// the smallest element of column "j_col"
  	vector<double>	extractRow_cond_minElement(int j_col);
	
    // Operations on elements
    
    void    setAllValues(double value);
	void	setValueFromMatrix(Matrix M);
	
	double  sumAllElements();
	double  sumLine(int i);		// sum all elements of line #i
	double  sumColumn(int j);	// sum all elements of line #i
	
	// conditional sum 
	double	sumColumnIf(int colToSum, int colToTest,
						double lowerBound, double upperBound);

	// counts nb of elements which are lower<element<upper  
	int		countColumnIf(int colToTest,
						  double lowerBound, double upperBound);
    
    void    setColumnValues(int colNb, vector<double> v);
	void	setRowValues(int rowNb_start0, vector<double> v);
	void	setRowValues(int rowNb_start0, vector<unsigned long> v);
    
    Matrix  transpose();
    
    bool    isSymetric();

    double  determinant();
	
	Matrix	getMinor(int row, int col);
	
	Matrix	inverse();
    
    Matrix  Cholesky();
    
    double  getMinimumValue();
    double  getMaximumValue();
	
};

Matrix operator + (Matrix &A,Matrix &B);
Matrix operator - (Matrix &A,Matrix &B);
Matrix operator * (Matrix &A,Matrix &B);
Matrix operator * (double a,Matrix &A);

Matrix Id(int n);

Matrix power(Matrix A,int n);

Matrix cholesky(Matrix A);	//renvoi la Matrix triangul L tq://si A symetrique,carre //L*transpo(L)=A

double distance_Matrix(Matrix A, Matrix B, double power);	// Euclidian distance b/w two matrices

Matrix rowBind(Matrix A, Matrix B);

Matrix	transpo(Matrix A);        // FIX ME : to delete if not used elsewhere
double	Det(Matrix A);           // FIX ME : to delete if not used elsewhere
int		test_sym(Matrix A);       // FIX ME : to delete if not used elsewhere



#endif
