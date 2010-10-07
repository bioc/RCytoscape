library (RUnit)
source ('unitTests/test_cytoscape.R')
#------------------------------------------------------------------------------------------------------------------------
reload.rcytoscape = function ()
{  
  if (length (grep ('RCytoscape', names (sessionInfo()$otherPkgs))) > 0)
    detach (package:RCytoscape)

  library (RCytoscape)

} # reload.rcytoscape
#------------------------------------------------------------------------------------------------------------------------
exper = function ()
{
  xml.rpc (cwe@uri, 'Cytoscape.discreteMapper', as.character (cwe@window.id), 'default', 'type', 'Node Color', '#FF0088', 
           c ('kinase', 'transcription factor'), # , 'glycoprotein'),
           c ('#0000FF', '#00FF00'))   # , '#8800FF'))

} #  exper
#------------------------------------------------------------------------------------------------------------------------
