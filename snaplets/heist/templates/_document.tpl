<apply template="base">
  
  <div class="row">
    <div class="col s8"> 
      <h3 id="cdaTitle" class="color-prim-3"><cdaTitle /></h3>
      <blockquote>
        <strong>Document dated: </strong><span id="docDD"><cdaDate /></span><br>
        <cdaAuthors>
        <strong>Document author: </strong><prefix /> <given /> <family /> <suffix /><br>
        <strong>Contact info:</strong><br>
        <streetAddressLine /> <state /><br>
        <country /> <postalCode /><br>
        <aTele />
        </cdaAuthors>
      </blockquote>
    </div>
    
    <div class="col s4">
      <div class="card-panel">
        <h5 class="color-prim-4">Patient</h5>
        <cdaPatients>
          <strong>Name: </strong><span class="pat-name .color-prim-4">
            <prefix /> <given /> <family /> <suffix /></span><br>
          <strong>Date of birth: </strong><span id="dofBirth"><pBirth /></span><br>
          <strong>Gender: </strong><pGender /><br>
          <strong>Race: </strong><pRace /> (<pEthnos />)<br>
          <br>
          <strong>Contacts information:</strong><br>
          <streetAddressLine /> <state /> <br>
          <country /> <postalCode /><br>
          <pTele />
        </cdaPatients>
      </div>
    </div>
  </div>    

  <cdaSections>
    <ul class="collapsible popout" data-collapsible="accordion">
      <cdaSection />
    </ul>
  </cdaSections>
  
  <pagination>
    <ul class="pagination">
    
      <li class="${shevLeft}"><a href="${prevPage}"><i class="material-icons">chevron_left</i></a></li>
      <pPages>
        <!--li class="${pageClass}"><a href="${pageUrl}"><pageText /></a></li-->
      </pPages>
      <li class="${shevRight}"><a href="${nextPage}"><i class="material-icons">chevron_right</i></a></li>

    </ul>
  </pagination>

</apply>
