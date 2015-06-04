C ======================================================================
      Subroutine vectorDOF(N, dof, vector)
C ======================================================================
C Pre-process input degrees of freedom
C ======================================================================
      implicit none
      include 'fem2Dtri.fd'

      Integer  N, dof(*), vector, i
      Logical  ifXnode
 
      vector = 0

      Do i = 1, N
         if(ifXnode(dof(i), VectorY)) vector = 1
         Call delXnode(dof(i), VectorY)
      End do

      Return
      End



C ======================================================================
      Subroutine listDOF(FEMtype, N, dof)
C ======================================================================
      implicit none
      include 'fem2Dtri.fd'
 
      Integer FEMtype, N, dof(*), i

      If(FEMtype.EQ.FEM_P0) Then
         N = 1
         dof(1) = Edof

      Else If(FEMtype.EQ.FEM_P1) Then
         N = 3
         Do i = 1, 3
            dof(i) = Vdof
         End do

      Else If(FEMtype.EQ.FEM_P2) Then
         N = 6
         Do i = 1, 3
            dof(i)   = Vdof
            dof(i+3) = Rdof
         End do

      Else If(FEMtype.EQ.FEM_P3) Then
         N = 10
         Do i = 1, 3
            dof(i)   = Vdof
            dof(i+3) = Rdof
            dof(i+6) = Rdof
         End do
         dof(10) = Edof
       
      Else If(FEMtype.EQ.FEM_P4) Then
         N = 15
         Do i = 1, 3
            dof(i)    = Vdof
            dof(i+ 3) = Rdof
            dof(i+ 6) = Rdof
            dof(i+ 9) = Rdof
            dof(i+12) = Edof
         End do
       
      Else If(FEMtype.EQ.FEM_P1vector) Then
         N = 6
         Do i = 1, 6
            dof(i) = Vdof
         End do

      Else If(FEMtype.EQ.FEM_P2vector) Then
         N = 12
         Do i = 1, 3
            dof(i)   = Vdof
            dof(i+3) = Rdof
            dof(i+6) = Vdof
            dof(i+9) = Rdof
         End do

      Else If(FEMtype.EQ.FEM_P2reduced) Then
         N = 9
         Do i = 1, 3
            dof(i)   = Vdof
            dof(i+3) = Rdof
            dof(i+6) = Vdof
         End do

      Else If(FEMtype.EQ.FEM_MINI) Then
         N = 8
         Do i = 1, 3
            dof(i)   = Vdof
            dof(i+4) = Vdof
         End do
         dof(4) = Edof
         dof(8) = Edof

      Else If(FEMtype.EQ.FEM_RT0) Then
         N = 3
         Do i = 1, 3
            dof(i) = RdofOrient
         End do

      Else If(FEMtype.EQ.FEM_BDM1) Then
         N = 6
         Do i = 1, 6
            dof(i) = RdofOrient
         End do

      Else If(FEMtype.EQ.FEM_CR1) Then
         N = 3
         Do i = 1, 3
            dof(i) = Rdof
         End do

      Else If(FEMtype.EQ.FEM_CR1vector) Then
         N = 6
         Do i = 1, 6
            dof(i) = Rdof
         End do

      Else
         N = 0
      End if

      Return 
      End



C ======================================================================
      Subroutine enumDOF(FEMtype, N, dof, i1)
C ======================================================================
C Local enumeration of degrees of freedom starting from the vertex i1.
C ======================================================================
      implicit none
      include 'fem2Dtri.fd'
 
      Integer FEMtype, N, dof(*), i1
      Integer iref(4), i2, i
      DATA    iref/1,2,3,1/

      i2 = i1
 
      If(FEMtype.EQ.FEM_P1) Then
         N = 3
         Do i = 1, 3
            dof(i) = i2
            i2 = iref(i2 + 1)
         End do

      Else If(FEMtype.EQ.FEM_P2) Then
         N = 6
         Do i = 1, 3
            dof(i)   = i2
            dof(i+3) = i2 + 3
            i2 = iref(i2 + 1)
         End do

      Else If(FEMtype.EQ.FEM_P3) Then
         N = 10
         Do i = 1, 3
            dof(i)   = i2
            dof(i+3) = i2 + 3
            dof(i+6) = i2 + 6
            i2 = iref(i2 + 1)
         End do
         dof(10) = 10
       
      Else If(FEMtype.EQ.FEM_P4) Then
         N = 15
         Do i = 1, 3
            dof(i)    = i2
            dof(i+ 3) = i2 + 3
            dof(i+ 6) = i2 + 6
            dof(i+ 9) = i2 + 9
            dof(i+12) = i + 12
            i2 = iref(i2 + 1)
         End do

      Else
         N = 0
      End if

      Return 
      End



C ======================================================================
      Integer Function orderDOF(FEMtype)
C ======================================================================
C Calculate maximal order for integration
C ======================================================================
      implicit none
      include 'fem2Dtri.fd'
 
      Integer FEMtype

      If(FEMtype.EQ.FEM_P0) Then
         orderDOF = 1

      Else If(FEMtype.EQ.FEM_P1) Then
         orderDOF = 1

      Else If(FEMtype.EQ.FEM_P2) Then
         orderDOF = 2

      Else If(FEMtype.EQ.FEM_P3) Then
         orderDOF = 3
       
      Else If(FEMtype.EQ.FEM_P4) Then
         orderDOF = 4
       
      Else If(FEMtype.EQ.FEM_P1vector) Then
         orderDOF = 1

      Else If(FEMtype.EQ.FEM_P2vector) Then
         orderDOF = 2

      Else If(FEMtype.EQ.FEM_P2reduced) Then
         orderDOF = 2

      Else If(FEMtype.EQ.FEM_MINI) Then
         orderDOF = 3

      Else If(FEMtype.EQ.FEM_RT0) Then
         orderDOF = 1

      Else If(FEMtype.EQ.FEM_BDM1) Then
         orderDOF = 2

      Else If(FEMtype.EQ.FEM_CR1) Then
         orderDOF = 1

      Else If(FEMtype.EQ.FEM_CR1vector) Then
         orderDOF = 1

      Else
         orderDOF = 1
      End if

      Return 
      End


