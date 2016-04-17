{ *************************************************************************** }
{                                                                             }
{ Audio Tools Library                                                         }
{ Programming tools for Borland Delphi 3, 4, 5, 6, 7, 2005                    }
{                                                                             }
{ http://mac.sourceforge.net/atl/                                             }
{ e-mail: macteam@users.sourceforge.net                                       }
{                                                                             }
{ Copyright (c) 2000-2002 by Jurgen Faul                                      }
{ Copyright (c) 2003-2005 by The MAC Team                                     }
{                                                                             }
{ Version 2.3 (27 May 2005)                                                   }
{                                                                             }
{ The pack includes several components described below:                       }
{                                                                             }
{ AAC            - for manipulating with AAC file information                 }
{ AC3            - for manipulating with AC3 file information                 }
{ APE Tag        - for manipulating with APE Tags                             }
{ CDDA Track     - for getting information for CDDA track                     }
{ DTS            - for manipulating with DTS file information                 }
{ FLAC           - for manipulating with FLAC file information                }
{ fpl            - reads foobar2000 playlist files (*.fpl)                    }
{ ID3v1          - for manipulating with ID3v1 tags                           }
{ ID3v2          - for manipulating with ID3v2 tags                           }
{ Monkey         - for manipulating with Monkey's Audio file information      }
{ MPEG Audio     - for manipulating with MPEG audio file information          }
{ Musepack       - for manipulating with Musepack file information            }
{ Ogg Vorbis     - for manipulating with Ogg Vorbis file information          }
{ OptimFROG      - for manipulating with OptimFROG file information           }
{ Speex          - for manipulating with Speex file information               }
{ TTA            - for manipulating with TTA file information                 }
{ TwinVQ         - for extracting information from TwinVQ file header         }
{ Vorbis Comment - for manipulating with Vorbis Comments                      }
{ WAV            - for manipulating with WAV files                            }
{ WavPack        - for manipulating with WAVPack Files                        }
{ WMA            - for extracting information from WMA file header            }
{                                                                             }
{ To compile, you need to have these components installed:                    }
{   - JEDI VCL 3.00                                                           }
{     http://jvcl.sourceforge.net                                             }
{   - TntWare Delphi Unicode Controls                                         }
{     http://www.tntware.com/delphicontrols/unicode/                          }
{                                                                             }
{ You are welcome to send bug reports, comments and suggestions.              }
{ Spoken languages: English, German.                                          }
{                                                                             }
{ 27.05.2005 - version 2.3                                                    }
{   - unicode file access support                                             }
{                                                                             }
{ 13.01.2005 - version 2.2                                                    }
{   - added AC3 component                                                     }
{   - added DTS component                                                     }
{   - updated APE Tag unit (writing support for APE 2.0 tags)                 }
{                                                                             }
{ 31.12.2004 - version 2.1                                                    }
{   - added TTA component                                                     }
{   - added Speex component                                                   }
{   - updated WavPack component                                               }
{   - added support for Lyrics3 v2.00 Tags to the ID3v1 component             }
{   - updated FLAC component                                                  }
{   - updated WAV component                                                   }
{   - updated MPEG Audio component                                            }
{   - some other updates/fixes to some components                             }
{                                                                             }
{ 14.06.2004 - version 2.0                                                    }
{   - added OptimFROG component                                               }
{   - added WavPack component                                                 }
{   - added Vorbis Comment component                                          }
{   - added fpl component                                                     }
{   - many changes/updates/fixes                                              }
{   - ATL is now released under the GNU LGPL license                          }
{                                                                             }
{ 04.11.2002                                                                  }
{   - TCDAtrack: first release                                                }
{   - TMPEGaudio: ability to recognize QDesign MPEG audio encoder             }
{   - TMPEGaudio: fixed bug with MPEG Layer II                                }
{   - TMPEGaudio: fixed bug with very big files                               }
{                                                                             }
{ 02.10.2002                                                                  }
{   - TAACfile: first release                                                 }
{   - TID3v2: added property TrackString                                      }
{   - TOggVorbis: writing support for Vorbis tag                              }
{   - TOggVorbis: changed several properties                                  }
{   - TOggVorbis: fixed bug with long Vorbis tag fields                       }
{                                                                             }
{ 13.08.2002                                                                  }
{   - TFLACfile: first release                                                }
{   - TTwinVQ: Added property Album                                           }
{   - TTwinVQ: Support for Twin VQ 2.0                                        }
{                                                                             }
{ 29.07.2002                                                                  }
{   - TMonkey: correction for calculating of duration                         }
{   - TID3v2: reading support for Unicode                                     }
{   - TID3v2: removed limitation for the track number                         }
{                                                                             }
{ 23.05.2002                                                                  }
{   - TMPEGaudio: improved reading performance (up to 50% faster)             }
{   - TID3v2: support for padding                                             }
{                                                                             }
{ 29.04.2002                                                                  }
{   - TWMAfile: first release                                                 }
{                                                                             }
{ 21.04.2002                                                                  }
{   - TAPEtag: first release                                                  }
{                                                                             }
{ 24.03.2002                                                                  }
{   - TID3v2: reading support for ID3v2.2.x & ID3v2.4.x tags                  }
{                                                                             }
{ 18.02.2002                                                                  }
{   - TOggVorbis: added property BitRateNominal                               }
{   - TOggVorbis: fixed bug with tag fields                                   }
{                                                                             }
{ 16.02.2002                                                                  }
{   - TID3v2: fixed bug with property Comment                                 }
{   - TID3v2: added info: composer, encoder, copyright, language, link        }
{                                                                             }
{ 08.02.2002                                                                  }
{   - TMPEGplus: fixed bug with property Corrupted                            }
{                                                                             }
{ 14.01.2002                                                                  }
{   - TWAVfile: fixed bug with calculating of duration                        }
{   - TWAVfile: some class properties added/changed                           }
{                                                                             }
{ 21.10.2001                                                                  }
{   - TOggVorbis: support for UTF-8                                           }
{   - TOggVorbis: fixed bug with vendor info detection                        }
{                                                                             }
{ 17.10.2001                                                                  }
{   - TID3v2: writing support for ID3v2.3.x tags                              }
{   - TID3v2: fixed bug with track number detection                           }
{   - TID3v2: fixed bug with tag reading                                      }
{                                                                             }
{ 09.10.2001                                                                  }
{   - TWAVfile: fixed bug with WAV header detection                           }
{                                                                             }
{ 11.09.2001                                                                  }
{   - TMPEGaudio: improved encoder guessing for CBR files                     }
{   - TMonkey: added property Samples                                         }
{   - TMonkey: removed WAV header information                                 }
{                                                                             }
{ 07.09.2001                                                                  }
{   - TMonkey: first release                                                  }
{                                                                             }
{ 31.08.2001                                                                  }
{   - TMPEGaudio: first release                                               }
{   - TID3v2: added public procedure ResetData                                }
{                                                                             }
{ 15.08.2001                                                                  }
{   - TOggVorbis: first release                                               }
{                                                                             }
{ 14.08.2001                                                                  }
{   - TID3v2: first release                                                   }
{                                                                             }
{ 06.08.2001                                                                  }
{   - TTwinVQ: first release                                                  }
{                                                                             }
{ 02.08.2001                                                                  }
{   - TMPEGplus: some class properties added/changed                          }
{                                                                             }
{ 31.07.2001                                                                  }
{   - TWAVfile: first release                                                 }
{                                                                             }
{ 26.07.2001                                                                  }
{   - TMPEGplus: fixed reading problem with "read only" files                 }
{                                                                             }
{ 25.07.2001                                                                  }
{   - TID3v1: first release                                                   }
{                                                                             }
{ 23.05.2001                                                                  }
{   - TMPEGplus: first release                                                }
{                                                                             }
{ This library is free software; you can redistribute it and/or               }
{ modify it under the terms of the GNU Lesser General Public                  }
{ License as published by the Free Software Foundation; either                }
{ version 2.1 of the License, or (at your option) any later version.          }
{                                                                             }
{ This library is distributed in the hope that it will be useful,             }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of              }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU           }
{ Lesser General Public License for more details.                             }
{                                                                             }
{ You should have received a copy of the GNU Lesser General Public            }
{ License along with this library; if not, write to the Free Software         }
{ Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA   }
{                                                                             }
{ *************************************************************************** }