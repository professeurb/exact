# Organisation mémoire

## Header

- head
- tail

### ColList

- col1, rowOfColPtr1
- col2, rowOfColPtr2
  ...
- coln, rowOfColPtrn

## Columns

### For each PRIMARY column,

- positionInColList
- endOfRowList ! it's the basic address of a column

#### RowList

- backLink1, row1
  ...
- backLinkN, rowN

## Rows

### For each row,

#### ReversedColList, only PrimaryCols

- backLinkN, colN
  ...
- backLink1, col1

- endOfReversedColList

#### RowList

- endOfRowList ! it's the basic address of a row

- backLink1, row1
  ...
- backLinkN, rowN
