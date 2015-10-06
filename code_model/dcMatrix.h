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

class dcMatrix
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
	
	//dcMatrix(){nbRows=0;nbCols=0;}
	dcMatrix(){}
	
	dcMatrix(int h,int l) 
	{
		nbRows=h; nbCols=l; 
		vector<double> tmp(l*h,0.0);
		val=tmp;
	}
	
	dcMatrix(int n) 
	{
		nbRows=n; nbCols=n;
		vector<double> tmp(n*n, 0.0);
		val=tmp;
	}
	
    dcMatrix(string pathFile);
	
	dcMatrix(vector<double> v);	// dcMatrix (n,1) from a single vector(n)
	
	
    
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
    
	void			removeRow(int i_row);	// removes row 'i_row' and resize dcMatrix
	void			removeCol(int j_col);	// removes column 'j_col' and resize dcMatrix

	
	
	// Extract the row #i of the matrix
	// "i" is calculated such that it is
	// the smallest element of column "j_col"
  	vector<double>	extractRow_cond_minElement(int j_col);
	
    // Operations on elements
    
    void    setAllValues(double value);
	void	setValueFromMatrix(dcMatrix M);
	
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
    
    dcMatrix  transpose();
    
    bool    isSymetric();

    double  determinant();
	
	dcMatrix	getMinor(int row, int col);
	
	dcMatrix	inverse();
    
    dcMatrix  Cholesky();
    
    double  getMinimumValue();
    double  getMaximumValue();
	
};

dcMatrix operator + (dcMatrix &A,dcMatrix &B);
dcMatrix operator - (dcMatrix &A,dcMatrix &B);
dcMatrix operator * (dcMatrix &A,dcMatrix &B);
dcMatrix operator * (double a,dcMatrix &A);

dcMatrix Id(int n);

dcMatrix power(dcMatrix A,int n);

dcMatrix cholesky(dcMatrix A);	//renvoi la dcMatrix triangul L tq://si A symetrique,carre //L*transpo(L)=A

double distance_Matrix(dcMatrix A, dcMatrix B, double power);	// Euclidian distance b/w two matrices

dcMatrix rowBind(dcMatrix A, dcMatrix B);

dcMatrix	transpo(dcMatrix A);        // FIX ME : to delete if not used elsewhere
double	Det(dcMatrix A);           // FIX ME : to delete if not used elsewhere
int		test_sym(dcMatrix A);       // FIX ME : to delete if not used elsewhere



#endif
