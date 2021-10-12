/*******************************************************************************
*
* (C) COPYRIGHT ABB Daimler-Benz Transportation Signal AB, 1998
*
* We reserve all rights in this file and in the information 
* contained therein. Reproduction, use or disclosure to third 
* parties without express authority is strictly forbidden.
*
* Module name: 
*
* %name: gp_a_mo.h %
* %version: 1 %
* %created_by: htoomasi %
* %date_created: 2014-09-23 13:17 %
* %Creation date of original object: Tue Feb 14 11:51:54 2006%
*
* Description: A header file
*
*******************************************************************************/
/*******************************************************************************
* Revision History
*
* Version  Date    Sign     Change description
*
*          100520  konhebn ATP_LOG only, some more features for mv_list
*          090910  konhebn  list also variable type 
*          090825  konhebn  features changed to be ATP_LOG only, task atpns_se#7540, improved logging after USF on target.
*          090129  konhebn  cleanup, task atpcu_se#26646
*          081001  konhebn  corrected some lint errors, CR atpcu_se#25308
* 1.6      080911  rquensel atpcu_se#24944, list load using mv_list command.
* 1.4-5    080909  konhebn  only ATP_LOG changes, added gpMoInt8pA, task atpns_se#6273
* 1.2-1.3  080514  konhebn  added sub class for free text.
* 1.1      080514  konhebn  Added classes for B (WIN32 version only) and removed some currently unused code.
* 1.0      080506  konhebn  some ATP_LOG stuff, task atpcu_se#23479, renamed from "mh_mv_a.hpp" to "gp_a_mo.h".
*
*******************************************************************************/

#ifndef GP_A_MO_H
#define GP_A_MO_H

#ifdef ATP_LOG

/*******************************************************************************
* Includes
*******************************************************************************/
//#include "mmi_virtual_common.hpp"
#include "vss_a_scalar_types.h"
#include "trc_stream.hpp"
#include "gp_a_bit_unpacker.hpp"
#include "gp_a_bit_packer.hpp"

 
/*******************************************************************************
* Macros
*******************************************************************************/

#ifndef NULL
#define NULL 0
#endif

#ifndef ASSERT
#define ASSERT(c) {if (!(c)) {bs_fatalFailureA(__FILE__,__LINE__,"assert",#c);}}
#endif

/* Gives the number of elements in an array */
#ifndef SIZE_OF_ARRAY
#define SIZE_OF_ARRAY(a) (sizeof(a)/sizeof(a[0]))
#endif



/*******************************************************************************
* Declarations and Definitions
*******************************************************************************/




class GpBitPackerA;
class GpBitUnpackerA;




void gpMoInitA(void);






class gpMoFolderA;








class gpMo_A
{
public:


  // Only negative values shall be used fo error codes
  // since values >=0 are used for number of bytes written etc.
  typedef enum {
    Ok = 0,
    BufferToSmall = -1,
    NotFound = -2,
    MemoryFull = -3,
    UnknownValue = -4,
    FailedToSet = -5,
    WriteFailed = -6,
    WrongStorageClass = -7,
    NoNvOrPaConfigured = -8,
    FeatureNotImplemented = -9,
    BelowRange = -10,
    AboveRange = -11,
    PackFailed = -12,
    NotInitialized = -13,
    WrongUsage = -14,
  } ReturnCode;


  explicit gpMo_A(const char *maName=NULL, gpMo_A* mhFolder=NULL, const char *descr=NULL, const char *longDescr=NULL);

  //void addToFolder(gpMoFolderA* mhFolder);



  inline const char *getName() const {return name_;};
  inline const char *getDescription() const {return description_;};
  inline const char *getLongDescription() const {return longDescription_;};
  const char* getPathAndName(char *str, int strSize) const;

  virtual int helpString(char *str, int strSize) const {if (strSize>0) {*str=0;}; return FeatureNotImplemented;}


  virtual int addSubObject(gpMo_A* /*mv*/) {return FeatureNotImplemented;};


  virtual int valueToString(char *str, int strSize) const;

  static int intToString(char *str, int strSize, int i);
  static int uintToString(char *str, int strSize, unsigned int i);
  static unsigned int uintFromString(const char *str);
  static int intFromString(const char *str);

  static gpMoFolderA* findOrCreateFolder(const char *folderName, gpMo_A* mhFolder);

  //virtual int size() const {return -1;};

  virtual int list(FileType* fp, const char* filter) const;

  virtual int listDiff(FileType* fp, const gpMo_A *otherVar) const;

  virtual gpMo_A *find(const char * const name);

  const gpMo_A *getFolder() const {return folder_;};

  bool isSameName(const gpMo_A *otherVar) const;

  virtual void listMap(FileType* fp, const unsigned char * const ptr, const unsigned int size) const;

  virtual void* getPointer() const {return NULL;}
  virtual int getVarSize() const {return 0;}
  virtual const char* getVarType() const {return "unknown";};

  gpMo_A *next_;

protected:
  // Copy constructor is private, it shall not be used since all
  // monitorable objects (mo) must have a unique name.
  gpMo_A(const gpMo_A& arg);

  void notifySubscribers() const;

  const char *name_; /* name of this object */
  const char *description_; /* a short description of this object, usually unit such as ms, cm/s ect */
  const char *longDescription_; /* a longer description of this object */


  gpMo_A *folder_; /* parent folder for this object, NULL if there is none */
  int folderIndex_;
};


class gpMoFolderA: public gpMo_A
{
public:
  gpMoFolderA(const char *folderName, gpMo_A* mhFolder=NULL, const char *mhDescription=0, const char *mhLongDescription=NULL);


  int valueToString(char *str, int strSize) const;

  int addSubObject(gpMo_A* monitorVariable);

  int list(FileType* fp, const char* filter) const;
  int listDiff(FileType* fp, const gpMo_A *otherVar) const;
  void listMap(FileType* fp, const unsigned char * const ptr, const unsigned int size) const;

  gpMo_A *find(const char * const name);

  int numberOfVariables() const;

protected:
  gpMo_A *firstSubObject_;
  gpMo_A *lastSubObject_;
  int nSubObjects_;
};




gpMoFolderA* gpMoDebugFolderInstance();




class gpMoInt32pA: public gpMo_A
{
public:
  gpMoInt32pA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int32A* pointer);

  //inline int size() const { return 32;};
  inline int32A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::intToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int32A);}
  const char* getVarType() const {return "int32A";};

protected:

  const int32A *pointer_;
};


class gpMoUint32pA: public gpMo_A
{
public:
  gpMoUint32pA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint32A* pointer);

  //inline int size() const { return 32;};
  inline uint32A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint32A);}
  const char* getVarType() const {return "uint32A";};

protected:

  const uint32A *pointer_;
};



class gpMoInt16pA: public gpMo_A
{
public:
  gpMoInt16pA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int16A* pointer);

  //inline int size() const { return 16;};
  inline int16A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::intToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int16A);}
  const char* getVarType() const {return "int16A";};

protected:

  const int16A *pointer_;
};



class gpMoUint16pA: public gpMo_A
{
public:
  gpMoUint16pA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint16A* pointer);

  //inline int size() const { return 16;};
  inline uint16A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint16A);}
  const char* getVarType() const {return "uint16A";};

protected:

  const uint16A *pointer_;
};


class gpMoInt8pA: public gpMo_A
{
public:
  gpMoInt8pA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int8A* pointer);

  //inline int size() const { return 8;};
  inline int8A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::intToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int8A);}
  const char* getVarType() const {return "int8A";};

protected:

  const int8A *pointer_;
};



class gpMoUint8pA: public gpMo_A
{
public:
  gpMoUint8pA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint8A* pointer);

  //inline int size() const { return 8;};
  inline uint8A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint8A);}
  const char* getVarType() const {return "uint8A";};

protected:

  const uint8A *pointer_;
};



class gpMoBoolpA: public gpMo_A
{
public:
  gpMoBoolpA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const boolA* pointer);

  //inline int size() const { return 8;};
  inline uint16A get() const { if (pointer_) {return *pointer_;} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(boolA);}
  const char* getVarType() const {return "boolA";};

protected:

  const boolA *pointer_;
};



class gpMoInt32aA: public gpMo_A
{
public:
  gpMoInt32aA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int32A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 32;};
  inline int32A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int32A)*nObjects_;}
  const char* getVarType() const {return "int32A[]";};

protected:

  const int32A *pointer_;
  int nObjects_;
};


class gpMoUint32aA: public gpMo_A
{
public:
  gpMoUint32aA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint32A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 32;};
  inline int32A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint32A)*nObjects_;}
  const char* getVarType() const {return "uint32A[]";};

protected:

  const uint32A *pointer_;
  int nObjects_;
};


class gpMoInt16aA: public gpMo_A
{
public:
  gpMoInt16aA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int16A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 16;};
  inline int16A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int16A)*nObjects_;}
  const char* getVarType() const {return "int16A[]";};

protected:

  const int16A *pointer_;
  int nObjects_;
};


class gpMoUint8aA: public gpMo_A
{
public:
  gpMoUint8aA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint8A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 8;};
  inline uint8A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint8A)*nObjects_;}
  const char* getVarType() const {return "uint8A[]";};

protected:

  const uint8A *pointer_;
  int nObjects_;
};




typedef int (*ToStringFuncPtrType)(char *, int, int);

class gpMoTextA: public gpMo_A
{
public:
  gpMoTextA(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, ToStringFuncPtrType func_ptr, int ref);

  //inline int size() const { return -1;};

  int valueToString(char *str, int strSize) const;

protected:

  ToStringFuncPtrType func_ptr_;
  int ref_;
};








class gpMoInt32pB: public gpMo_A
{
public:
  gpMoInt32pB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int32A* pointer);

  inline int size() const { return 32;};
  inline int32A get() const { if (pointer_) {return ~(*pointer_);} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::intToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int32A);}
  const char* getVarType() const {return "int32B";};


protected:

  const int32A *pointer_;
};


class gpMoUint32pB: public gpMo_A
{
public:
  gpMoUint32pB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint32A* pointer);

  inline int size() const { return 32;};
  inline uint32A get() const { if (pointer_) {return ~(*pointer_);} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint32A);}
  const char* getVarType() const {return "uint32B";};

protected:

  const uint32A *pointer_;
};



class gpMoInt16pB: public gpMo_A
{
public:
  gpMoInt16pB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int16A* pointer);

  inline int size() const { return 16;};
  inline int16A get() const { if (pointer_) {return ~(*pointer_);} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::intToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int16A);}
  const char* getVarType() const {return "int16B";};

protected:

  const int16A *pointer_;
};



class gpMoUint16pB: public gpMo_A
{
public:
  gpMoUint16pB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint16A* pointer);

  inline int size() const { return 16;};
  inline uint16A get() const { if (pointer_) {return ~(*pointer_);} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint16A);}
  const char* getVarType() const {return "uint16B";};

protected:

  const uint16A *pointer_;
};


class gpMoUint8pB: public gpMo_A
{
public:
  gpMoUint8pB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint8A* pointer);

  inline int size() const { return 8;};
  inline uint8A get() const { if (pointer_) {return ~(*pointer_);} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint8A);}
  const char* getVarType() const {return "uint8B";};

protected:

  const uint8A *pointer_;
};



class gpMoBoolpB: public gpMo_A
{
public:
  gpMoBoolpB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const boolB* pointer);

  inline int size() const { return 8;};
  inline uint16A get() const { if (pointer_) {return ~(*pointer_);} else {return 0;}};
  int valueToString(char *str, int strSize) const {return gpMo_A::uintToString(str, strSize, get());}
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(boolB);}
  const char* getVarType() const {return "boolB";};

protected:

  const boolB *pointer_;
};



class gpMoInt32aB: public gpMo_A
{
public:
  gpMoInt32aB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int32A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 32;};
  inline int32A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int32A)*nObjects_;}
  const char* getVarType() const {return "int32B[]";};

protected:

  const int32A *pointer_;
  int nObjects_;
};


class gpMoUint32aB: public gpMo_A
{
public:
  gpMoUint32aB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint32A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 32;};
  inline int32A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint32A)*nObjects_;}
  const char* getVarType() const {return "uint32B[]";};

protected:

  const uint32A *pointer_;
  int nObjects_;
};


class gpMoInt16aB: public gpMo_A
{
public:
  gpMoInt16aB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const int16A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 16;};
  inline int16A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(int16A)*nObjects_;}
  const char* getVarType() const {return "uint16B[]";};

protected:

  const int16A *pointer_;
  int nObjects_;
};


class gpMoUint8aB: public gpMo_A
{
public:
  gpMoUint8aB(const char *mhName, gpMo_A* mhFolder, const char *mhDescription, const uint8A* pointer, int nObjects);

  //inline int size() const { return nObjects_ * 8;};
  inline uint8A get(int index) const { if (pointer_) {return pointer_[index];} else {return 0;}};
  int valueToString(char *str, int strSize) const;
  void* getPointer() const {return (void*)pointer_;}
  int getVarSize() const {return sizeof(uint8A)*nObjects_;}
  const char* getVarType() const {return "uint8B[]";};

protected:

  const uint8A *pointer_;
  int nObjects_;
};





enum {
  MH_MV_LISTSIZE_A=2048,
  ERROR_STRING_SIZE=80
};



class gpMoList_A
{
public:

  typedef enum {
    BufferToSmall = gpMo_A::BufferToSmall,
  } ReturnCode;



  static TraceStream logAlways;
  static TraceStream logMajor;
  static TraceStream logMinor;
  static TraceStream logDetail;


  
  static int strncpyz(char *dst, const char *src, int n);
  static inline int strlen(const char *str) {int n=0; while (*str++) {++n;} return n;}; // Hopefully same as a normal strlen.
  static int atoi(const char *str);
  static int itoa(char *str, int value);





  static gpMoList_A*  instance ();


  gpMo_A *find(const char * const name) {return rootObject_.find(name);};


  void checkErrors(FileType* /*fp*/) const {;};

  int list(FileType* fp, bool listOnlyNonDefault, const char *filter) const;
  void get(FileType* fp, const char* mhName) const;
  void help(FileType* fp, const char* mhName) const;



private:
  gpMoList_A();


public:
  gpMoFolderA rootObject_;

};

#endif /* ATP_LOG */


#endif


 
/*************************** end of file **************************************/
