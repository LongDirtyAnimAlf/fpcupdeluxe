unit repoclient;
{ Generic repository client class. Implementations for hg, svn,... are availalbe
  Copyright (C) 2012-2013 Reinier Olislagers, Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  // Custom return codes
  FRET_LOCAL_REMOTE_URL_NOMATCH= -2;
  FRET_WORKING_COPY_TOO_OLD= -3;
  FRET_UNKNOWN_REVISION=-4; //for integer-based revision systems (svn)
  FRET_UNKNOWN_REVISIONSTRING='FRET_UNKNOWN_REVISIONSTRING'; //for string-based revision systems (hg, git,...)

type
  TRepoClient = class(TObject)
  protected
    FLocalRepository: string;
    FLocalRevision: integer;
    FRepositoryURL: string;
    FReturnCode: integer;
    FDesiredRevision: string;
    FVerbose: boolean;
    function GetLocalRevision: integer;
    procedure GetLocalRevisions;
    function GetRepoExecutable: string;
    // Makes sure non-empty strings have a / at the end.
    function IncludeTrailingSlash(AValue: string): string;
    procedure SetDesiredRevision(AValue: string);
    procedure SetLocalRepository(AValue: string);
    procedure SetRepositoryURL(AValue: string);
    procedure SetRepoExecutable(AValue: string);
    procedure SetVerbose(AValue: boolean);
  public
    //Performs a checkout/initial download
    //Note: it's often easier to call CheckOutOrUpdate
    procedure CheckOut;
    //Runs checkout if local repository doesn't exist, else does an update
    procedure CheckOutOrUpdate;
    //Creates diff of all changes in the local directory versus the SVN version
    function GetDiffAll:string;
    //Search for installed SVN executable (might return just a filename if in the OS path)
    function FindSVNExecutable: string;
    //Shows commit log for local directory
    procedure Log(var Log: TStringList);
    //Reverts/removes local changes so we get a clean copy again. Note: will remove modifications to files!
    procedure Revert;
    //Performs an SVN update (pull)
    //Note: it's often easier to call CheckOutOrUpdate; that also has some more network error recovery built in
    procedure Update;
    //Get/set desired revision to checkout/pull to (if none given, use HEAD)
    property DesiredRevision: string read FDesiredRevision write SetDesiredRevision;
    //Shows list of files that have been modified locally (and not committed)
    procedure LocalModifications(var FileList: TStringList);
    //Checks to see if local directory is a valid SVN repository for the repository URL given (if any)
    function LocalRepositoryExists: boolean;
    //Local directory that has an SVN repository/checkout.
    //When setting, relative paths will be expanded; trailing path delimiters will be removed
    property LocalRepository: string read FLocalRepository write SetLocalRepository;
    //Revision number of local repository: branch revision number if we're in a branch.
    property LocalRevision: integer read GetLocalRevision;
    //Parses output given by some commands (svn update, svn status) and returns files.
    // Files are marked by single characters (U,M,etc); you can filter on ore mor of these (pass [''] if not required).
    procedure ParseFileList(const CommandOutput: string; var FileList: TStringList; const FilterCodes: array of string);
    //URL where central (remote) SVN repository is placed
    property Repository: string read FRepositoryURL write SetRepositoryURL;
    //Exit code returned by last SVN client command; 0 for success. Useful for troubleshooting
    property ReturnCode: integer read FReturnCode;
    //Repository client executable. Can be set to explicitly determine which executable to use.
    property RepoExecutable: string read GetRepoExecutable write SetRepoExecutable;
    //Show additional console/log output?
    property Verbose:boolean read FVerbose write SetVerbose;
    constructor Create;
    destructor Destroy; override;
  end;


implementation

end.

