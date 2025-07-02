#' Beregn anbefalt fig.height for ggplot med spørsmålsbatteri
#'
#' Returnerer anbefalt høyde i tommer for en ggplot som viser et spørsmålsbatteri,
#' slik at y-etiketter (ofte lange spørsmålstekster) ikke klemmes sammen.
#'
#' @param n_antall_spm Antall spørsmål i batteriet (rader i plottet).
#' @param base_height Grunnhøyde i tommer for korte batterier (default: 4.5).
#' @param per_row Økning i tommer per ekstra rad (default: 0.1).
#' @param max_height Maksimum høyde i tommer (default: 12).
#'
#' @return Anbefalt høyde i tommer.
#' @export
#'
#' @examples
#' beregn_figheight(10)
#' beregn_figheight(20)
beregn_figheight <- function(n_antall_spm,
                             base_height = 4.5,
                             per_row = 0.1,
                             max_height = 12) {
  høyde <- base_height + (n_antall_spm * per_row)
  min(høyde, max_height)
}
