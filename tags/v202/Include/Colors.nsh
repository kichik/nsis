# Rob Segal
# Lobo Lunar
# Yathosho

!define WHITE "FFFFFF"
!define BLACK "000000"
!define YELLOW "FFFF00"
!define RED "FF0000"
!define GREEN "00FF00"
!define BLUE "0000FF"
!define MAGENTA "FF00FF"
!define CYAN "00FFFF"

!macro rgb2hex output R G B
IntFmt "${output}" "%02X" "${R}"
IntFmt "${output}" "${output}%02X" "${G}"
IntFmt "${output}" "${output}%02X" "${B}"
!macroend