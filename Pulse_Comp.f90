PROGRAM Pulse_com

IMPLICIT NONE

REAL*8 :: E_0                               !Amplitude
REAL*8 :: E                                 !Pulse
REAL*8 :: t_0                               !Pulse length

REAL*8, DIMENSION(:) :: phase               !Chirp phase
REAL*8 :: alpha                             !Chirp constant
REAL*8 :: alpha_pi                          !Chirp rate or FM rate, slope of chirp pulse
REAL*8 :: B_W = 15d+9                       !Bandwidth of chirp


REAL*8 :: f_c = 1275d+9                     !Central frequency
REAL*8 :: f                                 !Instatenuous frequency, differentiation of phase

REAL*8, DIMENSION(:) :: alpha               !Chirp constant

REAL*8 :: pi=ACOS(-1)

INTEGER :: t                                !Time
INTEGER :: t_0 = 35.2d-6                    ![s] Pulse length
INTEGER ::: t_0_2


INTEGER :: i,j,k

!     ++++++++++++++++
!++++++Initialization++++++
!     ++++++++++++++++

E = 0
E_0 = 100


alpha_phi = alpha / pi
alpha_phi = -4.3d+11  ![Hz/s]       

t_0_2 = t_0/2.0

alpha = alpha_pi * pi


DO t = -t_0_2, t_0_2


!     ++++++++++++++++++++++
!++++++phase of chipr pulse++++++
!     ++++++++++++++++++++++

      phase(t) = 2 * pi * f_c * t + alpha * (t **2)


!     +++++++++++++++++++++++++++++++++++     
!++++++frequency property of chirp pulse++++++
!     +++++++++++++++++++++++++++++++++++

      f(t) = f_c + (alpha/pi)* t            


!     +++++++++++++++++++++++++++
!++++++bandwidth of chirp puplse++++++
!     +++++++++++++++++++++++++++

      b_w = ABS(alpha) * t_0 / pi

  
!     +++++++++++++++++++++++++++++++++++++++++++
!++++++Frequency Modulation Pulse or Chirp Pulse++++++
!     +++++++++++++++++++++++++++++++++++++++++++

      E = E_0 * cos ( phase(t))

END DO



DO 
