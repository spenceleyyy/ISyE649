library(shiny)
library(base64enc)

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/three.js/r128/three.min.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/loaders/GLTFLoader.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/three@0.128.0/examples/js/controls/OrbitControls.js")
  ),
  
  titlePanel("Interactive 3D Brain Model"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Controls"),
      fileInput(
        "brain_file", 
        "Upload Different Brain Model (optional):",
        accept = c(".glb", ".gltf")
      ),
      hr(),
      checkboxInput(
        "auto_rotate", 
        "Auto Rotate", 
        value = FALSE
      ),
      sliderInput(
        "rotation_speed", 
        "Rotation Speed:", 
        min   = 0, 
        max   = 5, 
        value = 1, 
        step  = 0.1
      ),
      hr(),
      h4("Overlays"),
      selectInput(
        "eeg_channels",
        "EEG montage:",
        choices  = c("None" = "none",
                     "10-20 (19)" = "19",
                     "32 channels" = "32",
                     "64 channels" = "64",
                     "128 channels" = "128"),
        selected = "none"
      ),
      checkboxInput(
        "show_brodmann",
        "Show Brodmann overlays",
        value = FALSE
      ),
      hr(),
      p(strong("Mouse Controls:")),
      tags$ul(
        tags$li("Left click + drag: Rotate"),
        tags$li("Right click + drag: Pan"),
        tags$li("Scroll: Zoom")
      ),
      hr(),
      actionButton("reset_view", "Reset View", class = "btn-primary")
    ),
    
    mainPanel(
      tags$div(
        id = "canvas-container",
        style = "width: 100%; height: 600px; background-color: #f0f0f0; border: 1px solid #ccc; position: relative;",
        tags$canvas(id = "brain-canvas"),
        tags$div(
          id = "loading-message",
          style = "position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%); display: none;",
          h4("Loading model...")
        )
      )
    )
  ),
  
  tags$script(HTML('
    var scene, camera, renderer, controls, brain, animationId;
    var autoRotate = false;
    var rotationSpeed = 0.01;
    
    var eegSpheres = [];
    var brodmannMeshes = [];
    var brainCenter = new THREE.Vector3(0, 0, 0);
    var brainRadius = 2.3; // updated after loading
    var brainLoaded = false;

    function initScene() {
      var container = document.getElementById("canvas-container");
      var canvas = document.getElementById("brain-canvas");
      
      scene = new THREE.Scene();
      scene.background = new THREE.Color(0xf0f0f0);
      
      camera = new THREE.PerspectiveCamera(
        75,
        container.clientWidth / container.clientHeight,
        0.1,
        1000
      );
      camera.position.set(5, 0, 0);
      camera.lookAt(0, 0, 0);
      
      renderer = new THREE.WebGLRenderer({ canvas: canvas, antialias: true });
      renderer.setSize(container.clientWidth, container.clientHeight);
      renderer.setPixelRatio(window.devicePixelRatio);
      renderer.outputEncoding = THREE.sRGBEncoding;
      
      controls = new THREE.OrbitControls(camera, renderer.domElement);
      controls.enableDamping = true;
      controls.dampingFactor = 0.05;
      controls.screenSpacePanning = false;
      controls.minDistance = 2;
      controls.maxDistance = 10;
      
      // Lighting for shaded grey brain
      var hemiLight = new THREE.HemisphereLight(0xffffff, 0x444444, 0.7);
      hemiLight.position.set(0, 1, 0);
      scene.add(hemiLight);
      
      var directionalLight1 = new THREE.DirectionalLight(0xffffff, 1.0);
      directionalLight1.position.set(3, 5, 6);
      scene.add(directionalLight1);
      
      var directionalLight2 = new THREE.DirectionalLight(0xffffff, 0.6);
      directionalLight2.position.set(-4, 3, -5);
      scene.add(directionalLight2);
      
      var directionalLight3 = new THREE.DirectionalLight(0xffffff, 0.4);
      directionalLight3.position.set(0, -5, 2);
      scene.add(directionalLight3);
      
      window.addEventListener("resize", onWindowResize, false);
      
      animate();
    }

    // --------- EEG ELECTRODES (raycast to brain surface) ----------

    function clearEEGElectrodes() {
      for (var i = 0; i < eegSpheres.length; i++) {
        scene.remove(eegSpheres[i]);
      }
      eegSpheres = [];
    }

    function createEEGElectrodes(config) {
      clearEEGElectrodes();
      if (!config || config === "none") return;
      if (!brainLoaded || !brain) return;

      var n = parseInt(config);
      if (!isFinite(n) || n <= 0) return;

      var offset = 2.0 / n;
      var increment = Math.PI * (3.0 - Math.sqrt(5.0));

      for (var i = 0; i < n; i++) {
        var y = ((i * offset) - 1) + (offset / 2);
        var r = Math.sqrt(Math.max(0.0, 1 - y * y));
        var phi = i * increment;

        var x = Math.cos(phi) * r;
        var z = Math.sin(phi) * r;

        // direction on unit sphere
        var dir = new THREE.Vector3(x, y, z).normalize();

        // start ray outside brain, shoot toward center
        var origin = brainCenter.clone().add(dir.clone().multiplyScalar(brainRadius * 2.0));
        var rayDir = dir.clone().multiplyScalar(-1); // towards center

        var raycaster = new THREE.Raycaster(origin, rayDir, 0, brainRadius * 4.0);
        var intersects = raycaster.intersectObject(brain, true);

        var pos;
        if (intersects.length > 0) {
          pos = intersects[0].point.clone();
        } else {
          // fallback: just outside surface along direction
          pos = brainCenter.clone().add(dir.clone().multiplyScalar(brainRadius * 1.02));
        }

        var geom = new THREE.SphereGeometry(0.07, 16, 16);
        var mat = new THREE.MeshStandardMaterial({
          color: 0x3366ff,
          metalness: 0.1,
          roughness: 0.3
        });
        var sphere = new THREE.Mesh(geom, mat);
        sphere.position.copy(pos);

        eegSpheres.push(sphere);
        scene.add(sphere);
      }
    }

    // ---------- BRODMANN OVERLAYS ----------

    function clearBrodmann() {
      for (var i = 0; i < brodmannMeshes.length; i++) {
        scene.remove(brodmannMeshes[i]);
      }
      brodmannMeshes = [];
    }

    function createBrodmannOverlays() {
      clearBrodmann();
      if (!brainLoaded) return;

      var r = brainRadius * 0.7;

      var defs = [
        {
          name: "BA17 (Occipital visual)",
          color: 0x4b9cd3,
          pos: new THREE.Vector3(0, -0.3, -r)
        },
        {
          name: "BA4 (Motor)",
          color: 0xe67e22,
          pos: new THREE.Vector3(0.0, 0.4, 0.2 * r)
        },
        {
          name: "BA1-3 (Somatosensory)",
          color: 0x2ecc71,
          pos: new THREE.Vector3(0.0, 0.5, 0)
        },
        {
          name: "BA10 (Frontal pole)",
          color: 0x9b59b6,
          pos: new THREE.Vector3(0.0, 0.2, r)
        },
        {
          name: "BA22 (Temporal)",
          color: 0xe74c3c,
          pos: new THREE.Vector3(r * 0.6, -0.1, 0.1 * r)
        }
      ];

      defs.forEach(function(d) {
        var geom = new THREE.SphereGeometry(r * 0.35, 32, 32);
        var mat = new THREE.MeshStandardMaterial({
          color: d.color,
          transparent: true,
          opacity: 0.25,
          metalness: 0.0,
          roughness: 0.7
        });
        var mesh = new THREE.Mesh(geom, mat);
        mesh.position.copy(d.pos.clone().add(brainCenter));
        brodmannMeshes.push(mesh);
        scene.add(mesh);
      });
    }

    // --------- MODEL LOADING ----------

    function loadModel(fileDataUrl) {
      document.getElementById("loading-message").style.display = "block";
      
      if (brain) {
        scene.remove(brain);
        brain = null;
        brainLoaded = false;
        clearEEGElectrodes();
        clearBrodmann();
      }
      
      var loader = new THREE.GLTFLoader();
      
      loader.load(
        fileDataUrl,
        function(gltf) {
          brain = gltf.scene;
          
          brain.traverse(function(child) {
            if (child.isMesh) {
              if (child.geometry && child.geometry.computeVertexNormals) {
                child.geometry.computeVertexNormals();
              }
              child.material = new THREE.MeshStandardMaterial({
                color: 0xffffff,
                metalness: 0.05,
                roughness: 0.45
              });
              child.material.side = THREE.DoubleSide;
            }
          });
          
          var box = new THREE.Box3().setFromObject(brain);
          var center = box.getCenter(new THREE.Vector3());
          var size = box.getSize(new THREE.Vector3());
          
          var maxDim = Math.max(size.x, size.y, size.z);
          var scale = 4 / maxDim;
          brain.scale.multiplyScalar(scale);
          
          // recenter at origin
          brain.position.sub(center.multiplyScalar(scale));

          // approximate brain center and radius after scaling
          brainCenter.set(0, 0, 0);
          brainRadius = (Math.max(size.x, size.y, size.z) * scale) / 2.0;
          brainLoaded = true;

          scene.add(brain);
          document.getElementById("loading-message").style.display = "none";

          // after loading, re-apply any current UI choices
          if (window.Shiny && Shiny.shinyapp) {
            // Shiny will resend inputs via observers, so we just wait.
          }
        },
        undefined,
        function(error) {
          console.error("Error loading model:", error);
          document.getElementById("loading-message").innerHTML =
            "<h4 style=color:red;>Error loading model</h4>";
        }
      );
    }
    
    function animate() {
      animationId = requestAnimationFrame(animate);
      
      if (autoRotate && brain) {
        brain.rotation.y += rotationSpeed;
      }
      
      controls.update();
      renderer.render(scene, camera);
    }
    
    function onWindowResize() {
      var container = document.getElementById("canvas-container");
      camera.aspect = container.clientWidth / container.clientHeight;
      camera.updateProjectionMatrix();
      renderer.setSize(container.clientWidth, container.clientHeight);
    }
    
    function resetView() {
      camera.position.set(5, 0, 0);
      camera.lookAt(0, 0, 0);
      controls.reset();
    }
    
    document.addEventListener("DOMContentLoaded", function() {
      initScene();
    });
    
    if (typeof Shiny !== "undefined") {
      Shiny.addCustomMessageHandler("loadModel", function(message) {
        loadModel(message);
      });
      
      Shiny.addCustomMessageHandler("updateRotation", function(message) {
        autoRotate = message.auto;
        rotationSpeed = message.speed * 0.01;
      });
      
      Shiny.addCustomMessageHandler("resetView", function(message) {
        resetView();
      });

      Shiny.addCustomMessageHandler("setEEG", function(message) {
        var cfg = (message && message.n) ? message.n : "none";
        createEEGElectrodes(cfg);
      });

      Shiny.addCustomMessageHandler("setBrodmannVisible", function(message) {
        var show = !!(message && message.show);
        if (show) {
          createBrodmannOverlays();
        } else {
          clearBrodmann();
        }
      });
    }
  '))
)

server <- function(input, output, session) {
  # Auto-load a default brain model from Desktop as base64
  observeEvent(TRUE, {
    desktop_dir <- "~/Desktop"
    desktop_dir <- normalizePath(desktop_dir, winslash = "/", mustWork = FALSE)
    
    glb_files <- list.files(desktop_dir, pattern = "\\.glb$", full.names = TRUE)
    
    if (length(glb_files) == 0) {
      showNotification(
        paste("No .glb files found on Desktop at:", desktop_dir),
        type = "error"
      )
      return()
    }
    
    preferred <- grepl("Brain_MRI", basename(glb_files), ignore.case = TRUE)
    if (any(preferred)) {
      default_brain_path <- glb_files[which(preferred)[1]]
    } else {
      default_brain_path <- glb_files[1]
    }
    
    message("Using default brain model: ", default_brain_path)
    
    file_data   <- readBin(default_brain_path, "raw", file.info(default_brain_path)$size)
    file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
    session$sendCustomMessage("loadModel", file_base64)
  }, once = TRUE)
  
  # User-uploaded model
  observeEvent(input$brain_file, {
    req(input$brain_file)
    
    tryCatch({
      file_path   <- input$brain_file$datapath
      file_data   <- readBin(file_path, "raw", file.info(file_path)$size)
      file_base64 <- paste0("data:model/gltf-binary;base64,", base64encode(file_data))
      session$sendCustomMessage("loadModel", file_base64)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
    })
  })
  
  # Rotation control
  observe({
    session$sendCustomMessage(
      "updateRotation", 
      list(
        auto  = input$auto_rotate,
        speed = input$rotation_speed
      )
    )
  })
  
  # EEG montage selection
  observe({
    session$sendCustomMessage(
      "setEEG",
      list(n = input$eeg_channels)
    )
  })
  
  # Brodmann visibility
  observe({
    session$sendCustomMessage(
      "setBrodmannVisible",
      list(show = isTRUE(input$show_brodmann))
    )
  })
  
  # Reset view
  observeEvent(input$reset_view, {
    session$sendCustomMessage("resetView", TRUE)
  })
}

shinyApp(ui = ui, server = server)
