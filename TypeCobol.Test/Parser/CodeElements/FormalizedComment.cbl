*OK Usual usage case
*<< 
    - Description : My program description
    - parameters:
        - firstParam : firstParam description
        - SecondParam : SecondParam description
    - deprecated
    - replacedBy : MyFonction2
    - rEsTrIcTiOn : Do not Use BOOL var
    - need : be on debug mode
        - To have a hot full coffee mug
    - see : what you need to see
    - todo :
        - Add BOOL support 
        - implement a call counter
*>>
IDENTIFICATION DIVISION.
PROGRAM-ID. MyFonction.

*Ko Can not add a formalized Comment to a Data Divition Code Element
*<< Comment *>>
DATA DIVISION.
LOCAL-STORAGE SECTION.

*OK
*<< MyType *>>
01 myType TYPEDEF STRICT PUBLIC pic X(01).


*OK
*<< 
    - Desc : My program description
    - Params:
        - firstParam : firstParam description
        - SecondParam : SecondParam description
    - deprec : Can not handle BOOL variables. 
    - replBy : MyFonction2
    - RsTrIc : first line and
        second line for the same item
*>>
DECLARE FUNCTION currentDate PRIVATE RETURNING result TYPE Date.
PROCEDURE DIVISION.
