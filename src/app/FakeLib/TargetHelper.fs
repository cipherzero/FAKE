[<AutoOpen>]
module Fake.TargetHelper
    
open System
open System.Collections.Generic

type TargetTemplate<'a> =
    { Name: string;
      Dependencies: TargetTemplate<'a> list ref;
      Function : 'a -> unit}
   
type Target = TargetTemplate<unit>
   
/// TargetDictionary  
let TargetDict = new Dictionary<string,_>()

let AllTargets : Target list ref = ref [ { Name = String.Empty; Dependencies = ref []; Function = fun _ -> () } ]

/// Final Targets - stores final target and if it is activated
let FinalTargets = new Dictionary<_,_>()

/// The executed targets
let ExecutedTargets = new HashSet<_>()

/// The executed target time
let ExecutedTargetTimes = new List<_>()

/// Gets a target with the given name from the target dictionary
let getTarget name = 
//    match TargetDict.TryGetValue name with
//    | true, target -> target
//    | _  -> failwithf "Target \"%s\" is not defined." name
    name

/// Returns the DependencyString for the given target
let dependencyString target =
    if (!target.Dependencies).IsEmpty then String.Empty else
    !target.Dependencies 
      |> Seq.map (fun d -> d.Name)
      |> separated ", "
      |> sprintf "(==> %s)"

/// Returns a list with all targetNames
let getAllTargetsNames() = TargetDict |> Seq.map (fun t -> t.Key) |> Seq.toList
    
/// Do nothing - fun () -> ()   
let DoNothing = (fun () -> ())

/// Checks wether the dependency can be add
let checkIfDependencyCanBeAdd targetName dependentTargetName =
    let target = targetName
    let dependentTarget = getTarget dependentTargetName

    let rec checkDependencies dependentTarget =
        !dependentTarget.Dependencies 
          |> List.iter (fun dep ->
               if LanguagePrimitives.PhysicalEquality dep targetName then 
                  failwithf "Cyclic dependency between %s and %s" targetName.Name dependentTarget.Name
               checkDependencies (dep))
      
    checkDependencies dependentTarget
    target,dependentTarget

/// Adds the dependency to the front of the list of dependencies
let dependencyAtFront targetName dependentTargetName =
    let target,dependentTarget = checkIfDependencyCanBeAdd targetName dependentTargetName
    target.Dependencies := dependentTargetName :: !target.Dependencies
    
//    TargetDict.[targetName] <- { target with Dependencies = ref (dependentTargetName :: !target.Dependencies) }
  
/// Appends the dependency to the list of dependencies
let dependencyAtEnd targetName dependentTargetName =
    let target,dependentTarget = checkIfDependencyCanBeAdd targetName dependentTargetName
    target.Dependencies := !target.Dependencies @ [dependentTargetName] 
//    TargetDict.[targetName] <- { target with Dependencies = target.Dependencies @ [dependentTargetName] }

/// Adds the dependency to the list of dependencies
let dependency = dependencyAtEnd
  
/// Adds the dependencies to the list of dependencies  
let Dependencies targetName = List.iter (dependency targetName)

/// Dependencies operator
let inline (<==) x y = Dependencies x y

/// Set a dependency for all given targets
let TargetsDependOn target targets =
    !AllTargets
    |> Seq.toList  // work on copy since the dict will be changed
    |> List.filter (LanguagePrimitives.PhysicalEquality target >> not)
    |> List.filter (fun t -> Seq.exists (LanguagePrimitives.PhysicalEquality t) targets)
    |> List.iter (fun t -> dependencyAtFront t target)

/// Set a dependency for all registered targets
let AllTargetsDependOn target = !AllTargets |> TargetsDependOn target
  
/// Creates a target from template
let targetFromTemplate template name parameters =  
    let t = { Name = name; 
        Dependencies = ref [];
        Function = fun () ->
          // Don't run function now
          template.Function parameters }  
    AllTargets := t :: !AllTargets

    t <== !template.Dependencies
  
/// Creates a TargetTemplate with dependencies
let TargetTemplateWithDependecies dependencies body =
    { Name = String.Empty;
      Dependencies = dependencies;
      Function = body}     
        |> targetFromTemplate

/// Creates a TargetTemplate      
let TargetTemplate body = TargetTemplateWithDependecies (ref []) body 
  
/// Creates a Target
let Target name body = TargetTemplate body name ()  

type private BuildError(target, msg) =
    inherit System.Exception(sprintf "Stopped build! Error occured in target \"%s\".\r\nMessage: %s" target msg)
    member e.Target = target
    new (msg : string) = BuildError("[Unknown]", msg)

let mutable private errors = []   

let targetError targetName msg =
    closeAllOpenTags()
    errors <- BuildError(targetName, msg) :: errors
    traceError <| sprintf "Running build failed.\nError:\n%s" msg
    sendTeamCityError msg        
 
let addExecutedTarget target time =
    ExecutedTargets.Add target |> ignore
    ExecutedTargetTimes.Add(target,time) |> ignore

/// Runs all activated final targets (in alphabetically order)
let runFinalTargets() =
    FinalTargets
      |> Seq.filter (fun kv -> kv.Value)     // only if activated
      |> Seq.map (fun kv -> kv.Key)
      |> Seq.iter (fun name ->
           try             
               let watch = new System.Diagnostics.Stopwatch()
               watch.Start()
               tracefn "Starting Finaltarget: %s" name
               TargetDict.[name].Function()
               addExecutedTarget name watch.Elapsed
           with
           | exn -> targetError name (exn.ToString()))
              
/// <summary>Writes a dependency graph.</summary>
/// <param name="verbose">Whether to print verbose output or not.</param>
/// <param name="target">The target for which the dependencies should be printed.</param>
let PrintDependencyGraph verbose target =
    logfn "%sDependencyGraph for Target %s:" (if verbose then String.Empty else "Shortened ") target.Name
    let printed = new HashSet<_>()
    let order = new List<_>()
    let rec printDependencies indent act =
        let target = act
        let addToOrder = not (printed.Contains act)
        printed.Add act |> ignore
    
        if addToOrder || verbose then log <| (sprintf "<== %s" act.Name).PadLeft(3 * indent)
        Seq.iter (printDependencies (indent+1)) (!target.Dependencies)
        if addToOrder then order.Add act.Name
        
    printDependencies 0 target
    log ""
    log "The resulting target order is:"
    Seq.iter (logfn " - %s") order

/// <summary>Writes a build time report.</summary>
/// <param name="total">The total runtime.</param>
let WriteTaskTimeSummary total =    
    traceHeader "Build Time Report"
    let width = 
        ExecutedTargetTimes 
          |> Seq.map (fun (a,b) -> a.Length) 
          |> Seq.max
          |> max 8
    let aligned (name:string) duration = tracefn "%s   %O" (name.PadRight width) duration

    aligned "Target" "Duration"
    aligned "------" "--------"
    ExecutedTargetTimes
      |> Seq.iter (fun (name,time) -> aligned name time)

    aligned "Total:" total
    traceLine()

/// <summary>Runs a target and its dependencies</summary>
/// <param name="targetName">The target to run.</param>
let run targetName =            
    let rec runTarget targetName =
        try      
            if List.isEmpty errors (* && ExecutedTargets.Contains targetName |> not *) then
                let target = getTarget targetName      
                traceStartTarget target.Name (target.Name)
      
                List.iter runTarget (!target.Dependencies)
      
                if errors = [] then
                    let watch = new System.Diagnostics.Stopwatch()
                    watch.Start()
                    target.Function()
//                    addExecutedTarget targetName watch.Elapsed
                    traceEndTarget target.Name                
        with
        | exn -> targetError targetName.Name (exn.ToString())
      
    let watch = new System.Diagnostics.Stopwatch()
    watch.Start()        
    try
        WaitUntilEverythingIsPrinted()
        PrintDependencyGraph false targetName
        runTarget targetName
    finally
        runFinalTargets()
        WaitUntilEverythingIsPrinted()
        WriteTaskTimeSummary watch.Elapsed
        WaitUntilEverythingIsPrinted()
        List.iter raise errors
        WaitUntilEverythingIsPrinted()
 
/// Registers a final target (not activated)
let FinalTarget name body = 
    Target name body
    FinalTargets.Add(name,false)

/// Activates the FinalTarget
let ActivateFinalTarget name = 
    let t = getTarget name // test if target is defined
    FinalTargets.[name] <- true

/// Allows to use Tokens instead of strings
let (?) f s = f s

/// Allows to use Tokens instead of strings for TargetNames
let (?<-) f str action = f str action

/// Allows to use For? syntax for Dependencies
let For x y = x <== y

/// Converts a dependency into a list
let Dependency x = [x]

/// Appends the dependency to the list of dependencies
let And x y = y @ [x]

/// Runs a Target and its dependencies
let Run = run

type TargetBuilder() = 
    member this.YieldFrom(m) = fun () -> ()
    member this.Return(m) = fun () -> m
    member this.Combine(step1, step2) = 
        (fun () -> step1() |> step2)
    member this.Zero() =  ()
    member this.Delay(f) = f()

let target = TargetBuilder()

let Clean = target { CleanDirs ["test"]; CleanDirs ["test"] }

let Deploy = target {
        yield! [Clean]
    }


/// Runs the target given by the build script parameter or the given default target
//let RunParameterTargetOrDefault parameterName defaultTarget = getBuildParamOrDefault parameterName defaultTarget |> Run